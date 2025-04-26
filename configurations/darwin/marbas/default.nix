{ flake, lib, pkgs, config, specialArgs, ...}:
{
  imports = [
    flake.inputs.home-manager.darwinModules.home-manager
    flake.inputs.self.sharedModules.default
    flake.inputs.self.darwinModules.touch-id
    flake.inputs.stylix.darwinModules.stylix
    flake.inputs.self.nixosModules.bleeding
    flake.inputs.mac-app-util.darwinModules.default
  ];

  config = {
    inventoryHostName = "MacBook-Pro-van-Alexey";
    networking.hostName = "marbas";

    nixpkgs.config = {
      allowUnfree = true;
    };

    nixpkgs.flake.setNixPath = true;
    nixpkgs.flake.setFlakeRegistry = true;

    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

    hostConfig.features = [
      "interactive-cli"
      "emacs"
      "gui"
    ];

    # fonts.fontDir.enable = true;

    fonts.packages = with pkgs; [
      (nerdfonts.override { fonts = ["IosevkaTerm"]; })
    ];

    nixpkgs.overlays = [
      (final: prev: {
        emacs-pgtk-saved = prev.emacs-pgtk;
        emacs-saved = prev.emacs;
      })
      flake.inputs.emacs-overlay.overlays.default
    ];

    # system.defaults.dock.persistent-apps = [
    #   "/Applications/WezTerm.app"
    #   "/Users/binarin/.nix-profile/Applications/Emacs.app"
    #   "/Applications/Firefox.app"
    # ];

    nixpkgs.hostPlatform = "aarch64-darwin";
    nix.settings.experimental-features = "nix-command flakes";
    system.stateVersion = 5;
    environment.darwinConfig = "$HOME/personal-workspace/nixos-config";
    environment.systemPackages = with pkgs; [
      vim
      git
      # wezterm
      # firefox
      ripgrep
      openssh
      vscode
    ];

    environment.shells = with pkgs; [
      pkgs.bashInteractive pkgs.zsh
    ];

    home-manager.extraSpecialArgs = specialArgs;
    home-manager.useGlobalPkgs = true;
    home-manager.backupFileExtension = "backup";

    # So for small changes I can run only home-manager activation script, to reduce iteration time
    home-manager.useUserPackages = lib.mkForce false;

    home-manager.users.binarin = {osConfig, ...}: {
      imports = [
        flake.inputs.self.sharedModules.default
        flake.inputs.self.homeModules.git
        flake.inputs.self.homeModules.emacs
        flake.inputs.self.homeModules.wezterm
        flake.inputs.self.homeModules.interactive-cli
        flake.inputs.self.homeModules.impermanence
        flake.inputs.mac-app-util.homeManagerModules.default
      ];
      config = {
      	inherit (osConfig) hostConfig inventoryHostName;

        # XXX duplicate
        programs.git = {
          userName = "Alexey Lebedeff";
          userEmail = "binarin@binarin.info";
        };

        home.stateVersion = "24.11";
        home.packages = with pkgs; [
          # telegram-desktop
        ];

      };
    };

    users.users.binarin.home = "/Users/binarin";
    programs.bash.enable = true;
    programs.zsh.enable = true;
    programs.gnupg.agent.enable = true;
    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    programs.nix-index.enable = true;

    # services.emacs.enable = true;
    # services.emacs.package = ...;
    services.trezord.enable = true;
    services.tailscale.enable = true;

    system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = false;
    system.defaults.dock.show-recents = false;
    system.keyboard.enableKeyMapping = true;
    system.keyboard.remapCapsLockToControl = true;
    system.defaults.trackpad.TrackpadThreeFingerDrag = true;
    system.startup.chime = false;
    users.users.binarin.shell = pkgs.zsh;

    homebrew = {
      enable = true;
      global.autoUpdate = false;
      onActivation.upgrade = true;
      onActivation.autoUpdate = false;
      casks = [
        "alfred"
        "firefox"
        "freecad"
        "telegram"
        "prusaslicer"
        "orion"
      ];
      masApps = {
        # "Microsoft 365" = 1450038993;
        # "Microsoft Word" = 462054704;
      };
    };
  };
}
