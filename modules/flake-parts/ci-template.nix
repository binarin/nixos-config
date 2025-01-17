{self, inputs, lib, ...}:

let
  configurationsToBuild = builtins.attrNames self.nixosConfigurations;
  check-job = {
    runs-on = "native";
    steps = [
      {
        uses = ''actions/checkout@v4'';
      }
      {
        run = ''
          nix flake check
        '';
      }
    ];
  };
  build-all-configurations-job = {
    runs-on = "native";
    needs = [ "check" ];
    strategy = {
      fail-fast = false;
      matrix = {
        nixosConfiguration = builtins.attrNames self.nixosConfigurations;
      };
    };
    steps = [
      {
        uses = ''actions/checkout@v4'';
      }
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
      push = { branches = [ "master" ]; };
    };
    jobs = {
      check = check-job;
      nixos-configuration = build-all-configurations-job;
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
#      check = check-job;
      propose-inputs-update = {
        runs-on = "native";
#        needs = [ "check" ];
        steps = [
          {
            uses = "actions/checkout@v4";
            "with" = {
              ref = "master";
            };
          }
          {
            name = "Update flake inputs";
            run = "nix flake update";
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
        ] ++ (lib.forEach configurationsToBuild (cfg: {
          name = "Build nixosConfiguration.${cfg}";
          run = ''
              nix build "$(pwd)#nixosConfigurations.${cfg}.config.system.build.toplevel" \
                --keep-going \
                -j auto \
                -o "temp-result/${cfg}"
          '';
        })) ++ [
          {
            name = "Clean-up old GC roots";
            run = ''
              rm -rf "$HOME/.cache/nixos-config/proposed-update/nixos-configuration"
            '';
          }
        ] ++ (lib.forEach configurationsToBuild (cfg: {
          name = "Add nix-store GC root for nixosConfiguraion.${cfg}";
          run = ''
            nix-store --add-root "$HOME/.cache/nixos-config/proposed-update/nixos-configuration/${cfg}" \
              -r "$(readlink -f "temp-result/${cfg}")"
          '';
        })) ++ [
          {
            name = "Push to flake-bump branch";
            run = ''git push --force origin master:flake-bump'';
          }
        ];
      };
    };
  };

in {
  perSystem =
    { pkgs, ... }:
    let
      yaml = pkgs.formats.yaml { };
      master-yaml = yaml.generate "master.yaml" master-yaml-data;
      flake-update-yaml = yaml.generate "flake-update.yaml" flake-update-yaml-data;
    in {
      packages.ci-template-generator = pkgs.writeShellApplication {
        name = "ci-template-generator";
        runtimeInputs = with pkgs; [
          coreutils
          git
        ];
        text = ''
          git_wt=$(git rev-parse --show-toplevel)
          cat <<'EOF' > "$git_wt/.forgejo/workflows/master.yaml"
          # auto-generated via: nix run .#ci-template-generator
          EOF
          cat ${master-yaml} >> "$git_wt/.forgejo/workflows/master.yaml"

          cat <<'EOF' > "$git_wt/.forgejo/workflows/flake-update.yaml"
          # auto-generated via: nix run .#ci-template-generator
          EOF
          cat ${flake-update-yaml} >> "$git_wt/.forgejo/workflows/flake-update.yaml"
        '';
      };
    };
}
