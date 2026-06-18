{
  self,
  inputs,
  lib,
  ...
}:
let
  self = inputs.self;
  system = "x86_64-linux";

  deployPkgs = self.configured-pkgs."${system}".nixpkgs;

in
{
  flake = {
    options.deploy.nodes = lib.mkOption {
      type = with lib.types; lazyAttrsOf raw;
    };
    config = {
      lib.deploy-nixos = deployPkgs.deploy-rs.lib.activate.nixos;
      lib.deploy-home-manager = deployPkgs.deploy-rs.lib.activate.home-manager;
      lib.deploy-system-manager = base:
        deployPkgs.deploy-rs.lib.activate.custom base ''
          $PROFILE/bin/activate
          mapfile -t failed < <(systemctl list-units 'home-manager-*' --failed --no-legend --plain --no-pager 2>/dev/null | awk '{print $1}')
          if [[ ''${#failed[@]} -gt 0 ]]; then
            echo "FAILED home-manager units:"
            printf '  %s\n' "''${failed[@]}"
            for unit in "''${failed[@]}"; do
              echo "--- journalctl -u $unit ---"
              journalctl -u "$unit" --no-pager -n 40
            done
            exit 1
          fi
        '';

      # This is highly advised, and will prevent many possible mistakes
      perSystem =
        { system, ... }:
        {
          checks = self.configured-pkgs."${system}".nixpkgs.deploy-rs.lib.deployChecks self.deploy;
        };
    };
  };
}
