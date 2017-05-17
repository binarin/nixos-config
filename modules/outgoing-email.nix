{config, pkgs, ...}:

{
  security.wrappers.sendmail.source = "${pkgs.exim}/bin/sendmail";
  services.exim.enable = true;
  services.exim.config = ''
     qualify_domain = binarin.ru
     ${builtins.readFile ./exim.config}
  '';
}
