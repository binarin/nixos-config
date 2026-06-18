# Desktop Essentials Module

Extract a lightweight `homeModules.desktop-essentials` from the heavier `gui` module, containing packages useful on any interactive graphical machine (including murmur via standalone home-manager).

## New File: `modules/desktop-essentials.nix`

Defines `flake.homeModules.desktop-essentials`.

### Packages

| Category | Packages |
|----------|----------|
| Audio/media | `sox`, `brownnoise`, `mplayer`, `vlc` |
| Image | `geeqie`, `gimp`, `imagemagickBig` |
| PDF/docs | `evince`, `pdftk` |
| Bluetooth | `bluetui` |

### Mime Associations

```nix
xdg.mimeApps.defaultApplications = {
  "image/jpeg" = "geeqie.desktop";
  "application/pdf" = "org.gnome.Evince.desktop";
};
xdg.mimeApps.associations.added = {
  "application/pdf" = "org.gnome.Evince.desktop";
  "image/jpeg" = "geeqie.desktop";
};
```

### Module Key

`nixos-config.modules.home.desktop-essentials`

## Changes to Existing Modules

### `modules/gui.nix` (`homeModules.gui`)

- Add `self.homeModules.desktop-essentials` to imports
- Remove from `guiPackages`: `geeqie`, `gimp`, `imagemagickBig`, `evince`, `pdftk`, `mplayer`, `vlc`
- Remove `image/jpeg` and `application/pdf` mime associations (now in desktop-essentials)

### `modules/binarin/workstation.nix` (`homeModules.binarin-workstation`)

- Add `self.homeModules.desktop-essentials` to imports (unconditionally)
- Remove `sox` and `brownnoise` from `home.packages`

### `modules/machines/murmur.nix` (`homeModules.murmur-home-allebedev`)

- Add `self.homeModules.desktop-essentials` to imports
- Remove `brownnoise`, `sox`, `geeqie`, `gimp` from `home.packages`

## Design Decisions

- Home-manager only, no system-manager module. Bluetooth hardware/blueman on murmur is managed by Ubuntu natively; `bluetui` is just a TUI client.
- Mime associations move with their packages to avoid orphaned defaults.
- `workstation.nix` imports unconditionally since all workstation users are graphical (gated by `osConfig.services.graphical-desktop.enable` at the workstation level already).
