{
  self,
  ...
}:
{
  flake.modules.generic.public-keys =
    {
      lib,
      config,
      ...
    }:
    let
      allPublicKeys = import "${self}/inventory/public-keys.nix";
    in
    {
      key = "nixos-config.modules.generic.public-keys";

      config = {
        lib.publicKeys = {
          secureWithTag =
            tag:
            with lib;
            flip concatMap (attrValues allPublicKeys.ssh_keys) (
              {
                public_key,
                secure ? false,
                tags ? [ ],
                ...
              }:
              if secure && (elem tag tags) then [ public_key ] else [ ]
            );
          ssh.secureForUser =
            user:
            with lib;
            pipe allPublicKeys.ssh_keys [
              (mapAttrsToList (
                keyName:
                {
                  public_key,
                  secure ? false,
                  tags ? [ ],
                  description ? keyName,
                  ...
                }:
                let
                  matchTags =
                    if user == "root" then
                      [ "default" ]
                    else
                      [
                        "default"
                        "user-${user}"
                      ];
                in
                if secure && (intersectLists matchTags tags != [ ]) then "${public_key} ${description}" else null
              ))
              (filter (x: x != null))
            ];
        };
      };
    };

  flake.nixosModules.public-keys =
    { ... }:
    {
      key = "nixos-config.modules.nixos.public-keys";

      imports = [ self.modules.generic.public-keys ];
    };
}
