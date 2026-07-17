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
        talk = [ ];
        own = [ ];
        see = [ ];
      };
  };
}
