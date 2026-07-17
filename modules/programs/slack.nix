{ self, ... }:
{
  flake.overlays.slack = final: prev: {
    slack =
      let
        slackWithIcons = prev.slack.overrideAttrs (old: {
          installPhase = old.installPhase + ''
            local icons="$out/lib/slack/resources/app.asar.unpacked/dist/resources"
            cp "$icons/slack-taskbar-rest.ico" "$icons/slack-taskbar-unread.ico"
            cp "$icons/slack-taskbar-rest.ico" "$icons/slack-taskbar-highlight.ico"
          '';
        });
      in
      final.wrapShittyShit slackWithIcons {
        talk = [
          "org.freedesktop.Notifications" # desktop notifications
          "org.kde.StatusNotifierWatcher" # system tray: register the item
        ];
        # Electron/Chromium owns org.kde.StatusNotifierItem-<pid>-<id> for its
        # tray icon. xdg-dbus-proxy wildcards only match on '.' boundaries, so
        # the '-<pid>-<id>' suffix can't be targeted precisely; org.kde.* is the
        # tightest pattern that covers it.
        own = [ "org.kde.*" ];
        see = [ ];
      };
  };
}
