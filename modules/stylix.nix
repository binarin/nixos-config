{
  self,
  inputs,
  ...
}:
let
  sharedConfig =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      imports = [
        self.modules.generic.zenburn
      ];

      stylix.enable = true;
      stylix.autoEnable = false;

      stylix.image =
        let
          # One of the Windows 95 default colors
          wallpaper = pkgs.runCommand "image.png" { } ''
            ${pkgs.imagemagick}/bin/magick -size 1920x1080 "xc:#00807F" $out
          '';
        in
        wallpaper;

      stylix.fonts = {
        sizes = {
          terminal = 14;
          desktop = 10;
          applications = 12;
        };

        serif = {
          package = pkgs.noto-fonts;
          name = "Noto Serif Regular";
        };

        sansSerif = {
          package = pkgs.inter;
          name = "Inter Regular";
        };

        monospace = {
          package = pkgs.nerd-fonts.iosevka-term;
          name = "IosevkaTerm Nerd Font";
        };

        emoji = {
          package = pkgs.noto-fonts-emoji;
          name = "Noto Color Emoji";
        };
      };

      stylix.base16Scheme = with config.zenburn.colors; {
        scheme = "Zenburn";
        slug = "zenburn";

        base02 = bg_minus_05; # Selection Background
        base00 = bg; # Default Background
        base01 = bg_plus_05; # Lighter Background (Used for status bars, line number and folding marks)
        base07 = bg_plus_1; # Light Background (Not often used)

        base04 = fg_minus_05; # Dark Foreground (Used for status bars)
        base05 = fg; # Default Foreground, Caret, Delimiters, Operators
        base06 = fg_plus_1; # Light Foreground (Not often used)

        base03 = green; # Comments, Invisibles, Line Highlighting
        base08 = orange; # Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
        base09 = green_minus_4; # Integers, Boolean, Constants, XML Attributes, Markup Link Url
        base0A = red_plus_2; # Classes, Markup Bold, Search Text Background
        base0B = red; # Strings, Inherited Class, Markup Code, Diff Inserted
        base0C = blue_plus_1; # Support, Regular Expressions, Escape Characters, Markup Quotes
        base0D = cyan; # Functions, Methods, Attribute IDs, Headings
        base0E = yellow; # Keywords, Storage, Selector, Markup Italic, Diff Changed
        base0F = magenta; # Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>        h

        # interestingly enough, for a given ANSI color emacs has
        # possibility to pick different colors depending on it being
        # used as a foreground or as a background. That's why "if
        # possible ..."  annotations are here.
        base10 = bg; # ansi black  - if possible pref bg color 'bg-1'
        base11 = red_minus_2; # ansi red    - if possible pref bg 'red-4'
        base12 = green_plus_2; # ansi green  - if possible pref bg 'green+2', then this one is just 'green'
        base13 = orange; # ansi yellow - if possible pref bg 'yellow'
        base14 = blue_minus_1; # ansi blue   - if possible pref bg 'blue-4'
        base15 = magenta; # ansi purple - if possible pref bg 'red'
        base16 = cyan; # ansi cyan   - if possible pref bg 'blue'
        base17 = fg; # ansi white  - if possible pref bg 'fg-1'
      };

      lib.style.template =
        name: template: data:
        pkgs.stdenv.mkDerivation {
          name = "${name}";

          nativeBuildInpts = [ pkgs.mustache-go ];

          # Pass Json as file to avoid escaping
          passAsFile = [ "jsonData" ];
          jsonData = builtins.toJSON {
            inherit data;
            zenburn = config.zenburn.colors;
            fonts = {
              inherit (config.stylix.fonts) sizes;
            }
            // lib.genAttrs [
              "serif"
              "sansSerif"
              "monospace"
              "emoji"
            ] (nm: config.stylix.fonts."${nm}".name);
          };

          # Disable phases which are not needed. In particular the unpackPhase will
          # fail, if no src attribute is set
          phases = [
            "buildPhase"
            "installPhase"
          ];

          buildPhase = ''
            ${pkgs.mustache-go}/bin/mustache $jsonDataPath ${template} > rendered_file
          '';

          installPhase = ''
            cp rendered_file $out
          '';
        };
    };
in
{
  flake-file.inputs = {
    stylix.url = "github:danth/stylix/release-25.05";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.nixosModules.stylix =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      key = "nixos-config.modules.nixos.stylix";

      imports = [
        inputs.stylix.nixosModules.stylix
        sharedConfig
      ];

      disabledModules = [ "${inputs.stylix}/modules/regreet/nixos.nix" ];

      config = lib.mkMerge [
        {
          stylix.homeManagerIntegration.followSystem = false;
          stylix.homeManagerIntegration.autoImport = false;

          home-manager.sharedModules = [
            self.homeModules.stylix
            sharedConfig
          ];
        }
        (lib.mkIf config.services.graphical-desktop.enable {
          # XXX stylix.targets.lightdm.enable = true;
          stylix.targets.gtk.enable = true;

          # XXX Another copy in home/stylix.nix
          stylix.cursor = {
            package = pkgs.bibata-cursors;
            name = "Bibata-Modern-Amber";
            size = 16;
          };
          # XXX stylix.targets.chromium.enable = true;
        })
      ];
    };

  flake.homeModules.stylix =
    {
      config,
      lib,
      pkgs,
      osConfig,
      ...
    }:
    {
      key = "nixos-config.modules.home.stylix";

      imports = [ inputs.stylix.homeModules.stylix ];

      config = lib.mkMerge [
        {
          # XXX Another copy in nixos/stylix.nix
          stylix.cursor = {
            package = pkgs.bibata-cursors;
            name = "Bibata-Modern-Amber";
            size = 16;
          };

          stylix.targets =
            lib.genAttrs
              [
                "bat"
                "tmux"
                "btop"
                "fzf"
                "zellij"
              ]
              (_nm: {
                enable = true;
              });
        }
        (lib.mkIf osConfig.services.graphical-desktop.enable {
          stylix.targets =
            lib.genAttrs
              [
                "swaync"
                "vscode"
                "firefox"
                "gtk"
              ]
              (_nm: {
                enable = true;
              });
        })
      ];
    };
}
