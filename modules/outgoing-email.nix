{config, pkgs, ...}:

{
  security.setuidPrograms = [ "sendmail" ];
  services.exim.enable = true;
  services.exim.config = ''
     qualify_domain = binarin.ru
     ${builtins.readFile ./exim.config}
  '';
}
