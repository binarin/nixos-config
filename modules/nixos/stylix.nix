{flake, config, lib, pkgs, ...}:
let
  # One of the Windows 95 default colors
  wallpaper = pkgs.runCommand "image.png" {} ''
    ${pkgs.imagemagick}/bin/magick -size 1920x1080 "xc:#00807F" $out
  '';

  # with # so that rainbow-mode can recognize them
  emacsZenburnColors = {
    "fg-1"     = "#656555";
    "fg-05"    = "#989890";
    "fg"       = "#DCDCCC";
    "fg+1"     = "#FFFFEF";
    "fg+2"     = "#FFFFFD";
    "bg-2"     = "#000000";
    "bg-1"     = "#2B2B2B";
    "bg-08"    = "#303030";
    "bg-05"    = "#383838";
    "bg"       = "#3F3F3F";
    "bg+05"    = "#494949";
    "bg+1"     = "#4F4F4F";
    "bg+2"     = "#5F5F5F";
    "bg+3"     = "#6F6F6F";
    "red-6"    = "#6C3333";
    "red-5"    = "#7C4343";
    "red-4"    = "#8C5353";
    "red-3"    = "#9C6363";
    "red-2"    = "#AC7373";
    "red-1"    = "#BC8383";
    "red"      = "#CC9393";
    "red+1"    = "#DCA3A3";
    "red+2"    = "#ECB3B3";
    "orange"   = "#DFAF8F";
    "yellow-2" = "#D0BF8F";
    "yellow-1" = "#E0CF9F";
    "yellow"   = "#F0DFAF";
    "green-5"  = "#2F4F2F";
    "green-4"  = "#3F5F3F";
    "green-3"  = "#4F6F4F";
    "green-2"  = "#5F7F5F";
    "green-1"  = "#6F8F6F";
    "green"    = "#7F9F7F";
    "green+1"  = "#8FB28F";
    "green+2"  = "#9FC59F";
    "green+3"  = "#AFD8AF";
    "green+4"  = "#BFEBBF";
    "cyan"     = "#93E0E3";
    "blue+3"   = "#BDE0F3";
    "blue+2"   = "#ACE0E3";
    "blue+1"   = "#94BFF3";
    "blue"     = "#8CD0D3";
    "blue-1"   = "#7CB8BB";
    "blue-2"   = "#6CA0A3";
    "blue-3"   = "#5C888B";
    "blue-4"   = "#4C7073";
    "blue-5"   = "#366060";
    "magenta"  = "#DC8CC3";
  };
  emacsZenburnFaces = {
    font-lock-builtin-face = { fg = "fg"; bold = true; };
    font-lock-comment-face = { fg = "green"; };
    font-lock-comment-delimiter-face = { fg = "green-2"; };
    font-lock-constant-face = { fg = "zenburn-green+4"; };
    font-lock-doc-face = { fg = "green+2"; };
    font-lock-function-name-face = { fg = "cyan"; };
    font-lock-keyword-face = { fg = "yellow"; bold = true; };
    font-lock-negation-char-face = { fg = "yellow"; bold = true; };
    font-lock-preprocessor-face  = { fg = "blue+1"; };
    font-lock-regexp-grouping-construct = { fg = "yellow"; bold = true; };
    font-lock-regexp-grouping-backslash = { fg = "green";  bold = true; };
    font-lock-string-face = { fg = "red"; };
    font-lock-type-face = { fg = "blue-1"; };
    font-lock-variable-name-face = { fg = "orange"; };
    font-lock-warning-face = { fg = "yellow-2"; bold = true; };
  };
in
{
  imports = [
    flake.inputs.stylix.nixosModules.stylix
  ];
  disabledModules = [
    "${flake.inputs.stylix}/modules/regreet/nixos.nix"
  ];

  options = {
    # Emacs Zenburn colors
    zenburn = lib.mapAttrs (nm: val: lib.mkOption { default = val; type = lib.types.str; description = "Zenburn color ${nm}";}) emacsZenburnColors;
  };

  config = {
    environment.systemPackages = with pkgs; [
      fontpreview
      inter
    ];

    stylix.enable = true;
    stylix.autoEnable = false;

    stylix.base16Scheme = let
      inherit (config) zenburn;
    in {
      scheme = "Zenburn";
      slug = "zenburn";

      base02 = zenburn."bg-05";   # Selection Background
      base00 = zenburn."bg";      # Default Background
      base01 = zenburn."bg+05";   # Lighter Background (Used for status bars, line number and folding marks)
      base07 = zenburn."bg+1";    # Light Background (Not often used)

      base04 = zenburn."fg-05";   # Dark Foreground (Used for status bars)
      base05 = zenburn."fg";      # Default Foreground, Caret, Delimiters, Operators
      base06 = zenburn."fg+1";   # Light Foreground (Not often used)

      base03 = zenburn."green";   # Comments, Invisibles, Line Highlighting
      base08 = zenburn."orange";  # Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
      base09 = zenburn."green-4"; # Integers, Boolean, Constants, XML Attributes, Markup Link Url
      base0A = zenburn."red+2";   # Classes, Markup Bold, Search Text Background
      base0B = zenburn."red";     # Strings, Inherited Class, Markup Code, Diff Inserted
      base0C = zenburn."blue+1";  # Support, Regular Expressions, Escape Characters, Markup Quotes
      base0D = zenburn."cyan";    # Functions, Methods, Attribute IDs, Headings
      base0E = zenburn."yellow";  # Keywords, Storage, Selector, Markup Italic, Diff Changed
      base0F = zenburn."magenta"; # Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>        h

      # interestingly enough, for a given ANSI color emacs has
      # possibility to pick different colors depending on it being
      # used as a foreground or as a background. That's why "if
      # possible ..."  annotations are here.
      base10 = zenburn."bg";      # ansi black  - if possible pref bg color 'bg-1'
      base11 = zenburn."red-2";   # ansi red    - if possible pref bg 'red-4'
      base12 = zenburn."green+2"; # ansi green  - if possible pref bg 'green+2', then this one is just 'green'
      base13 = zenburn."orange";  # ansi yellow - if possible pref bg 'yellow'
      base14 = zenburn."blue-1";  # ansi blue   - if possible pref bg 'blue-4'
      base15 = zenburn."magenta"; # ansi purple - if possible pref bg 'red'
      base16 = zenburn."cyan";    # ansi cyan   - if possible pref bg 'blue'
      base17 = zenburn."fg";      # ansi white  - if possible pref bg 'fg-1'
    };

    stylix.image = wallpaper;

    stylix.fonts = {
      sizes = {
        terminal = 12;
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
        package = pkgs.nerdfonts.override {fonts = [ "IosevkaTerm" ]; };
        name = "IosevkaTerm Nerd Font";
      };

      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };

    stylix.cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Amber";
      size = 24;
    };

    stylix.targets.lightdm.enable = true;
    stylix.targets.gtk.enable = true;
    stylix.targets.chromium.enable = true;
  };
}
