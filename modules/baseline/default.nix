{ ... }:
{
  flake.nixosModules.baseline =
    { ... }:
    {
      i18n.defaultLocale = "nl_NL.UTF-8";
      i18n.extraLocales = [
        "nl_NL.UTF-8/UTF-8"
        "ru_RU.UTF-8/UTF-8"
      ];
    };
}
