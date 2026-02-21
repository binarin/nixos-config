{ self, lib, ... }:

let
  configurationsToBuild = builtins.attrNames self.nixosConfigurations;

  checkout-and-unlock =
    {
      ref ? null,
    }:
    [
      (
        {
          uses = "actions/checkout@v4";
        }
        // lib.optionalAttrs (ref != null) {
          "with".ref = ref;
        }
      )
      {
        name = "unlock git-crypt";
        env.GIT_CRYPT_KEY = "\${{ secrets.GIT_CRYPT_KEY }}";
        run = ''
          git crypt unlock <(echo "$GIT_CRYPT_KEY"|base64 -d)
        '';
      }
    ];

  check-job = {
    runs-on = "native";
    needs = [ "build-all-configurations-job" ];
    steps = (checkout-and-unlock { }) ++ [
      {
        run = ''
          nix flake check
        '';
      }
    ];
  };

  build-all-configurations-job = {
    runs-on = "native";
    strategy = {
      fail-fast = false;
      matrix = {
        nixosConfiguration = builtins.attrNames self.nixosConfigurations;
      };
    };
    steps = (checkout-and-unlock { }) ++ [
      {
        run = ''
          nix build "$(pwd)#nixosConfigurations.''${{ matrix.nixosConfiguration }}.config.system.build.toplevel" \
            --keep-going \
            -j auto \
            -o "$HOME/.cache/nixos-config/master/nixos-configuration/''${{ matrix.nixosConfiguration }}"
        '';
      }
    ];
  };

  master-yaml-data = {
    on = {
      push = {
        branches = [ "master" ];
      };
      pull_request = {
        types = [
          "opened"
          "synchronize"
          "reopened"
        ];
      };
    };
    jobs = {
      nixos-configuration = build-all-configurations-job;
      check = check-job;
    };
  };

  docker-update-yaml-data = {
    on = {
      schedule = [
        { cron = "42 11 * * *"; } # Daily at 11:42
      ];
      workflow_dispatch = { };
    };
    jobs = {
      propose-docker-updates = {
        runs-on = "native";
        steps = (checkout-and-unlock { ref = "master"; }) ++ [
          {
            name = "Set git username for commits";
            run = ''git config user.name "Docker Image Updater" '';
          }
          {
            name = "Set git email for commits";
            run = ''git config user.email "docker-updater@binarin.info"'';
          }
          {
            name = "API auth";
            run = ''set -x; fj -H forgejo.lynx-lizard.ts.net auth logout forgejo.lynx-lizard.ts.net || true; echo "''${{ secrets.PR_TOKEN }}" | fj -H forgejo.lynx-lizard.ts.net auth add-key nixos-config-bumper'';
          }
          {
            name = "Check for docker image updates and create PRs";
            run = ''
              set -euo pipefail

              # Run the check script in write mode
              nix run .#check-arion-images -- --write || true

              # Get list of changed JSON files
              changed_files=$(git diff --name-only -- '*.json' || true)

              if [[ -z "$changed_files" ]]; then
                echo "No docker image updates found"
                exit 0
              fi

              echo "Found updates in: $changed_files"

              # Process each changed file
              for json_file in $changed_files; do
                # Extract project name from filename (e.g., modules/machines/homebox.json -> homebox)
                project=$(basename "$json_file" .json)
                branch="docker-update-$project"

                echo "Processing $project..."

                # Stage just this file
                git add "$json_file"

                # Commit
                git commit -m "Update docker image versions for $project"

                # Push to branch (force to overwrite existing)
                git push --force origin "HEAD:$branch"

                # Create or update PR
                fj -H forgejo.lynx-lizard.ts.net pr create \
                  -r binarin/nixos-config \
                  --base master \
                  --head "$branch" \
                  --body "Automated docker image version bump for $project" \
                  "Update docker images: $project" || true

                # Reset back: undo the commit but keep changes staged
                git reset --soft HEAD~1
                # Unstage all changes
                git reset HEAD

                echo "Created/updated PR for $project"
              done

              echo "Done processing all updates"
            '';
          }
        ];
      };
    };
  };

  iso-wifi-yaml-data = {
    on = {
      workflow_dispatch = { };
    };
    jobs = {
      build-iso-wifi = {
        runs-on = "native";
        "if" = "github.ref == 'refs/heads/master'";
        steps = (checkout-and-unlock { ref = "master"; }) ++ [
          {
            name = "Build ISO image";
            run = ''
              nix build "$(pwd)#nixosConfigurations.iso.config.system.build.isoImage" \
                -j auto \
                -o iso-result
            '';
          }
          {
            name = "Inject WiFi credentials and create GC root";
            run = ''
              set -euo pipefail

              # Read WiFi password from git-crypt decrypted file
              WIFI_PASSWORD=$(cat files/agares-guest.git-crypt)

              # Find the ISO file
              ISO_FILE=$(find iso-result -name "*.iso" | head -1)

              # Prepare output directory
              OUTPUT_DIR="$HOME/.cache/nixos-config/master/iso-wifi"
              mkdir -p "$OUTPUT_DIR"

              # Run injection script
              ./scripts/inject-iso-wifi.sh "$ISO_FILE" "agares-guest" "$WIFI_PASSWORD" "$OUTPUT_DIR/nixos-wifi.iso"

              echo "WiFi-enabled ISO created at: $OUTPUT_DIR/nixos-wifi.iso"
            '';
          }
        ];
      };
    };
  };

  flake-update-yaml-data = {
    on = {
      schedule = [
        { cron = "13 10 * * Mon"; }
      ];
      workflow_dispatch = {
      };
    };
    jobs = {
      propose-inputs-update = {
        runs-on = "native";
        steps =
          (checkout-and-unlock { ref = "master"; })
          ++ [
            {
              name = "Update flake inputs";
              run = "nix flake update";
            }
            {
              name = "give autofollow a chance to mess with flake.lock";
              run = "nix run .#write-flake";
            }
            {
              name = "Set git username for commits";
              run = ''git config user.name "Automatic Flake Updater" '';
            }
            {
              name = "Set git email for commits";
              run = ''git config user.email "flake-updater@binarin.info"'';
            }
            {
              # Do it early, so the flake will get a proper git id
              name = "Commit updates";
              run = ''git commit --allow-empty -am "Bump inputs"'';
            }
          ]
          ++ (lib.forEach configurationsToBuild (cfg: {
            name = "Build nixosConfiguration.${cfg}";
            run = ''
              nix build "$(pwd)#nixosConfigurations.${cfg}.config.system.build.toplevel" \
                --keep-going \
                -j auto \
                -o "temp-result/${cfg}"
            '';
          }))
          ++ [
            {
              name = "Run flake check";
              run = "nix flake check";
            }
            {
              name = "Clean-up old GC roots";
              run = ''
                rm -rf "$HOME/.cache/nixos-config/proposed-update/nixos-configuration"
              '';
            }
          ]
          ++ (lib.forEach configurationsToBuild (cfg: {
            name = "Add nix-store GC root for nixosConfiguraion.${cfg}";
            run = ''
              nix-store --add-root "$HOME/.cache/nixos-config/proposed-update/nixos-configuration/${cfg}" \
                -r "$(readlink -f "temp-result/${cfg}")"
            '';
          }))
          ++ [
            {
              name = "Push to flake-bump branch";
              run = "git push --force origin master:flake-bump";
            }
            {
              name = "API auth";
              run = ''set -x; fj -H forgejo.lynx-lizard.ts.net auth logout forgejo.lynx-lizard.ts.net || true; echo "''${{ secrets.PR_TOKEN }}" | fj -H forgejo.lynx-lizard.ts.net auth add-key nixos-config-bumper'';
            }
            {
              name = "Maybe create PR";
              run = ''fj -H forgejo.lynx-lizard.ts.net pr create -r binarin/nixos-config --base master --head flake-bump --body "Bump flake inputs" "Bump everything" || true'';
            }
          ];
      };
    };
  };

in
{
  perSystem =
    { pkgs, ... }:
    let
      makeYaml =
        name: value:
        pkgs.callPackage (
          { runCommand, remarshal }:
          runCommand name
            {
              nativeBuildInputs = [ remarshal ];
              value = builtins.toJSON value;
              passAsFile = [ "value" ];
              preferLocalBuild = true;
            }
            ''
              cat <<'EOF' > "$out"
              # auto-generated via: nix run .#ci-template-generator
              EOF
              json2yaml --yaml-style-newline '|' "$valuePath" >> "$out"
            ''
        ) { };
    in
    {
      packages.ci-template-generator = pkgs.writeShellApplication {
        name = "ci-template-generator";
        runtimeInputs = with pkgs; [
          coreutils
          git
        ];
        text = ''
          git_wt=$(git rev-parse --show-toplevel)
          cat "${makeYaml "master.yaml" master-yaml-data}" > "$git_wt/.forgejo/workflows/master.yaml"
          cat "${makeYaml "flake-update.yaml" flake-update-yaml-data}" > "$git_wt/.forgejo/workflows/flake-update.yaml"
          cat "${makeYaml "docker-update.yaml" docker-update-yaml-data}" > "$git_wt/.forgejo/workflows/docker-update.yaml"
          cat "${makeYaml "iso-wifi.yaml" iso-wifi-yaml-data}" > "$git_wt/.forgejo/workflows/iso-wifi.yaml"
        '';
      };
    };
}
