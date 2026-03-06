{
  self,
  inputs,
  ...
}:
{
  flake.nixosModules.nix-builder =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.nixos-config.nix-builder;
      runnerIndices = lib.range 1 cfg.runnerCount;
      runnerSuffix = n: if n == 1 then "" else "-${toString n}";
      runnerName = n: "nixos-config${runnerSuffix n}";
      # Service names use \x2d for hyphens in systemd escaping
      serviceEscapedName = n: "gitea-runner-nixos\\x2dconfig${runnerSuffix n}.service";
    in
    {
      key = "nixos-config.modules.nixos.nix-builder";
      imports = [
        "${inputs.srvos}/nixos/roles/nix-remote-builder.nix"

        self.nixosModules.impure-nix-setup
      ];

      options.nixos-config.nix-builder = {
        runnerCount = lib.mkOption {
          type = lib.types.ints.positive;
          default = 2;
          description = "Number of Forgejo CI runners to create on this machine";
        };
      };

      config = {
        nixos-config.export-metrics.enable = true;

        nix.settings.system-features = [
          "big-parallel"
        ];

        nix.extraOptions = ''
          build-dir = /nix/build
        '';

        users.users.nix-remote-builder.openssh.authorizedPrincipals = lib.forEach [
          "nix-remote-builder"
          "binarin"
          "root"
        ] (k: ''restrict,command="nix-daemon --stdio" ${k}'');
        roles.nix-remote-builder.schedulerPublicKeys = [ ];

        sops.secrets."nixos-config-runner-token" = {
          restartUnits = map serviceEscapedName runnerIndices;
        };

        sops.templates.nixos-config-runner-token-env-file.content = ''
          TOKEN=${config.sops.placeholder."nixos-config-runner-token"}
        '';

        services.gitea-actions-runner = {
          package = pkgs.forgejo-runner;
          instances =
            let
              commonConfig = {
                enable = true;
                url = "https://forgejo.lynx-lizard.ts.net";
                labels = [ "native:host" ];
                hostPackages = with pkgs; [
                  bash
                  coreutils
                  curl
                  forgejo-cli
                  gawk
                  gitMinimal
                  git-crypt
                  gnused
                  jq
                  just
                  nix
                  nodejs
                  wget
                ];
              };
              mkRunner = n: {
                name = runnerName n;
                value = commonConfig // {
                  name = if n == 1 then config.networking.hostName else "${config.networking.hostName}-${toString n}";
                  tokenFile = config.sops.templates.nixos-config-runner-token-env-file.path;
                };
              };
            in
            builtins.listToAttrs (map mkRunner runnerIndices);
        };
      };
    };
}
