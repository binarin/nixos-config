{self, inputs, lib, ...}:

let
  master-yaml-data = {
    on = {
      push = { branches = [ "master" ]; };
    };
    jobs = {
      check = {
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

      nixos-configuration = {
        runs-on = "native";
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
    };
  };

in {
  perSystem =
    { pkgs, ... }:
    let
      yaml = pkgs.formats.yaml { };
      master-yaml = yaml.generate "master.yaml" master-yaml-data;
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
        '';
      };
    };
}
