{ self, ... }:
{
  flake.nixosModules.age-encryption =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.nixos.age-encryption";

      services.pcscd.enable = true;

      environment.systemPackages = with pkgs; [
        age
        age-plugin-yubikey
      ];
    };

  flake.homeModules.age-encryption =
    { pkgs, ... }:
    {
      key = "nixos-config.modules.home.age-encryption";

      imports = [
        self.homeModules.impermanence
      ];

      impermanence.persist-directories = [
        ".config/sops/age"
      ];
    };
}
