{
  network.description = "personal servers";
  kodi = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-kodi
     ];
  };
  balthamel = {config, lib, pkgs, ...}: {
     imports = [
       ../configuration.nix-balthamel
     ];
  };
  naberius = {config, lib, pkgs, ...}: {
    imports = [
      ../configuration.nix-naberius
    ];
    deployment.keys = {
      gitlab-database-password = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/database-password";
      };
      gitlab-db-key-base = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/db_key_base";
      };
      gitlab-otp-key-base = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/otp_key_base";
      };
      gitlab-secret-key-base = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/secret_key_base";
      };
      gitlab-naberius-runner-token = {
        text = builtins.extraBuiltins.pass "deploy/gitlab/naberius_runner_token";
      };
      nix-store-secret = {
        text = builtins.extraBuiltins.pass "deploy/naberius-nix-store/secret";
        destDir = "/var/lib/nix-serve";
        user = "nix-serve";
      };
    };
  };
  amon = {config, lib, pkgs, ...}: {
    imports = [
      ../configuration.nix-amon
    ];
  };
}
