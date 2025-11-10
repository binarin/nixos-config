{
  self,
  ...
}:
{
  flake.modules.generic.flake-files =
    {
      pkgs,
      lib,
      ...
    }:
    {
      key = "nixos-config.modules.generic.flake-files";

      config = {
        lib.self =
          let
            read = name: builtins.readFile "${self}/files/${name}";
          in
          {
            inherit read;
            file = name: pkgs.writeText name (read name);
            script = name: pkgs.writeScript name (read name);
            scriptBin = name: pkgs.writeScriptBin name (read name);
            file' =
              name:
              pkgs.writeText (lib.replaceStrings [ "/" ] [ "__" ] name) (builtins.readFile "${self}/${name}");

            optionalFile' =
              name:
              let
                shortName = lib.replaceStrings [ "/" ] [ "__" ] name;
                path = "${self}/${name}";
                text = if builtins.pathExists path then builtins.readFile path else "";
              in
              "${pkgs.writeText shortName text}";

            base64Dir =
              name:
              with lib;
              let
                cleanName = lib.replaceStrings [ "/" ] [ "__" ] name;
                dirContents = pipe "${self}/files/${name}" [
                  builtins.readDir
                  (filterAttrs (_k: v: v == "regular"))
                  attrNames
                  (filter (hasSuffix ".base64"))
                  (imap1 (
                    idx: base64File: {
                      idx = toString idx;
                      fileName = removeSuffix ".base64" base64File;
                      fileContent = read "${name}/${base64File}";
                    }
                  ))
                ];
                fileNames = pipe dirContents [
                  (map (
                    { idx, fileName, ... }:
                    {
                      name = "FILENAME_${idx}";
                      value = fileName;
                    }
                  ))
                  listToAttrs
                ];
                fileContents = pipe dirContents [
                  (map (
                    { idx, fileContent, ... }:
                    {
                      name = "FILE_CONTENT_${idx}";
                      value = fileContent;
                    }
                  ))
                  listToAttrs
                ];
              in
              pkgs.runCommand cleanName
                (
                  {
                    passAsFile = attrNames fileContents;
                    NUM_ITEMS = toString (length (attrNames fileContents));
                  }
                  // fileNames
                  // fileContents
                )
                ''
                  mkdir $out
                  for idx in $(seq 1 $NUM_ITEMS); do
                    nameVar="FILENAME_$idx"
                    contentVar="FILE_CONTENT_''${idx}Path"
                    base64 -d < "''${!contentVar}" > "$out/''${!nameVar}"
                  done
                '';
          };
      };
    };

  flake.nixosModules.flake-files =
    { config, ... }:
    {
      key = "nixos-config.modules.nixos.flake-files";

      imports = [ self.modules.generic.flake-files ];

      config = {
        home-manager.sharedModules = [ self.modules.generic.flake-files ];
      };
    };

  nixosSharedModules = [ self.nixosModules.flake-files ];
}
