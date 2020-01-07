{config, pkgs, lib, ...}:

let secrets = lib.importJSON ./bkng-kerberos.secret.json;
in
{
  krb5 = {
    enable = true;
    libdefaults = {
      default_realm = secrets.default_realm;
      dns_lookup_realm = "true";
      dns_lookup_kdc = "true";
    };
    domain_realm = secrets.domain_realm;
  };
  programs.chromium.extraOpts = secrets.extraOpts;
  environment.systemPackages = [ pkgs.krb5Full ];
}
