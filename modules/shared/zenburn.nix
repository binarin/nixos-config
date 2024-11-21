{flake, lib, config, pkgs, ...}:

let
  # with `#` so that rainbow-mode can recognize them
  # -/+ replaces with _minus_/_plus_ to be able to use them with the same name everywhere
  emacsZenburnColors = {
    "fg_minus_1"     = "#656555";
    "fg_minus_05"    = "#989890";
    "fg"             = "#DCDCCC";
    "fg_plus_1"      = "#FFFFEF";
    "fg_plus_2"      = "#FFFFFD";
    "bg_minus_2"     = "#000000";
    "bg_minus_1"     = "#2B2B2B";
    "bg_minus_08"    = "#303030";
    "bg_minus_05"    = "#383838";
    "bg"             = "#3F3F3F";
    "bg_plus_05"     = "#494949";
    "bg_plus_1"      = "#4F4F4F";
    "bg_plus_2"      = "#5F5F5F";
    "bg_plus_3"      = "#6F6F6F";
    "red_minus_6"    = "#6C3333";
    "red_minus_5"    = "#7C4343";
    "red_minus_4"    = "#8C5353";
    "red_minus_3"    = "#9C6363";
    "red_minus_2"    = "#AC7373";
    "red_minus_1"    = "#BC8383";
    "red"            = "#CC9393";
    "red_plus_1"     = "#DCA3A3";
    "red_plus_2"     = "#ECB3B3";
    "orange"         = "#DFAF8F";
    "yellow_minus_2" = "#D0BF8F";
    "yellow_minus_1" = "#E0CF9F";
    "yellow"         = "#F0DFAF";
    "green_minus_5"  = "#2F4F2F";
    "green_minus_4"  = "#3F5F3F";
    "green_minus_3"  = "#4F6F4F";
    "green_minus_2"  = "#5F7F5F";
    "green_minus_1"  = "#6F8F6F";
    "green"          = "#7F9F7F";
    "green_plus_1"   = "#8FB28F";
    "green_plus_2"   = "#9FC59F";
    "green_plus_3"   = "#AFD8AF";
    "green_plus_4"   = "#BFEBBF";
    "cyan"           = "#93E0E3";
    "blue_plus_3"    = "#BDE0F3";
    "blue_plus_2"    = "#ACE0E3";
    "blue_plus_1"    = "#94BFF3";
    "blue"           = "#8CD0D3";
    "blue_minus_1"   = "#7CB8BB";
    "blue_minus_2"   = "#6CA0A3";
    "blue_minus_3"   = "#5C888B";
    "blue_minus_4"   = "#4C7073";
    "blue_minus_5"   = "#366060";
    "magenta"        = "#DC8CC3";
  };

  emacsZenburnFaces = {
    default                             = { fg = "fg"; bg = "bg"; };
    font_lock_builtin_face              = { fg = "fg"; bold = true; };
    font_lock_comment_face              = { fg = "green"; };
    font_lock_comment_delimiter_face    = { fg = "green_minus_2"; };
    font_lock_constant_face             = { fg = "green_plus_4"; };
    font_lock_doc_face                  = { fg = "green_plus_2"; };
    font_lock_function_name_face        = { fg = "cyan"; };
    font_lock_keyword_face              = { fg = "yellow"; bold = true; };
    font_lock_negation_char_face        = { fg = "yellow"; bold = true; };
    font_lock_preprocessor_face         = { fg = "blue_plus_1"; };
    font_lock_regexp_grouping_construct = { fg = "yellow"; bold = true; };
    font_lock_regexp_grouping_backslash = { fg = "green";  bold = true; };
    font_lock_string_face               = { fg = "red"; };
    font_lock_type_face                 = { fg = "blue_minus_1"; };
    font_lock_variable_name_face        = { fg = "orange"; };
    font_lock_warning_face              = { fg = "yellow_minus_2"; bold = true; };
  };
in {
  options = {
    zenburn = {
      colors = lib.mapAttrs (nm: val: lib.mkOption { default = val; type = lib.types.str; description = "Zenburn color ${nm}";}) emacsZenburnColors;
      faces = lib.mapAttrs (nm: val: with lib; with types;
        optionalAttrs (val ? "fg") {
          fg = mkOption { type = nullOr str; description = "Face foreground color - #XXXXXX"; default = emacsZenburnColors."${val.fg}"; };
        } // optionalAttrs (val ? "bold") {
          bold = mkoption { type = bool; description = "Is the face bold?"; default = val.bold; };
        } // optionalAttrs (val ? "bg") {
          bg = mkOption { type = nullOr str; description = "Face background color - #XXXXXX"; default = emacsZenburnColors."${val.bg}"; };
        }
      );
      cssVars.text = with lib; pipe emacsZenburnColors [
        attrsToList
        (map ({name, value}: "--zenburn_${name}: ${value};"))
        (lst: [":root {"] ++ lst ++ ["}"])
        (concatStringsSep "\n")
        (css: mkOption { type = types.str; description = "CSS vars like --zenburn_bg_minus_1 as text"; default = css;})
      ];
      cssVars.file = lib.mkOption {
        type = with lib.types; oneOf [ path package ];
        default = pkgs.writeText "zenburn-colors.css" config.zenburn.cssVars.text;
      };
    };
  };
  config = {
  };
}
