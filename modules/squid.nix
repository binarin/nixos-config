{config, lib, pkgs, ...}:

with lib;

let
 cacheDir = "/var/cache/squid";
 logDir = "/var/log/squid";
 cacheLog = "${logDir}/cache.log";
 accessLog = "${logDir}/access.log";

 configFile = pkgs.writeText "squid.conf" ''
   cache_dir ufs ${cacheDir} 10000 16 256
   coredump_dir ${cacheDir}
   cache_log ${cacheLog}
   access_log daemon:${accessLog} squid

   ${config.services.squid.config}
 '';

 ensureLogFilesService = let
   script = pkgs.writeScript "ensure-squid-logs" ''
     #!${pkgs.bash}/bin/bash
     set -euo pipefail
     mkdir -p ${logDir}
     chown -R squid ${logDir}
   '';
 in {
   description = "Ensures that squid log directory exists and has proper permisisons";
   after = [ "local-fs.target" ];
   serviceConfig.Type = "oneshot";
   serviceConfig.ExecStart = script;
 };

 ensureCacheDirsService = {
   description = "Ensure squid cache directories exists";
   after = [ "local-fs.target" "squid-log-files.service" ];
   requires = [ "squid-log-files.service" ];
   path = [ pkgs.squid ];
   serviceConfig.Type = "oneshot";
   serviceConfig.User = "squid";
   serviceConfig.ExecStart = ''
     ${pkgs.squid}/bin/squid -N -z -f ${configFile}
   '';
 };
 squidService = {
   description = "Squid HTTP Cache";
   after = [ "network-interfaces.target" "squid-cache-dirs.service" ];
   requires = [ "squid-cache-dirs.service" ];
   wantedBy = [ "multi-user.target" ];
   serviceConfig.ExecStart = "${pkgs.squid}/bin/squid -N -f ${configFile}";
   serviceConfig.Type = "simple";
   serviceConfig.User = "squid";
   serviceConfig.Restart = "always";
 };
in
{
  options = {
    services.squid.enable = mkOption {
      default = false;
      type = types.bool;
    };
    services.squid.config = mkOption {
      default = "";
      type = types.lines;
    };
  };
  config = mkIf config.services.squid.enable {
    services.logrotate.enable = true;
    services.logrotate.config = ''
      ${accessLog} ${cacheLog} {
        daily
        compress
        create 644 squid nogroup
        delaycompress
        missingok
        rotate 7
      }
    '';
    systemd.services = {
      squid-cache-dirs = ensureCacheDirsService;
      squid-log-files = ensureLogFilesService;
      squid = squidService;
    };
    environment.systemPackages = [ pkgs.squid ];
    users.extraUsers.squid = {
      isNormalUser = false;
      home = cacheDir;
      createHome = true;
      shell = "${pkgs.bash}/bin/bash";
    };
  };
}
