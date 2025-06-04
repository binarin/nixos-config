local wezterm = require 'wezterm'

local stylix = require 'stylix-vars'

local config = wezterm.config_builder()
local act = wezterm.action

config.font = wezterm.font(stylix.fontName)
config.font_size = stylix.fontSize

config.unix_domains = {
  {
    name = 'unix',
  },
}

config.default_gui_startup_args = { 'connect', 'unix' }

-- Show which key table is active in the status area
wezterm.on('update-right-status', function(window, pane)
  local name = window:active_key_table()
  if name then
    name = 'TABLE: ' .. name
  end
  window:set_right_status(name or '')
end)

config.keys = {
   { key = 'Enter', mods = 'ALT', action = act.DisableDefaultAssignment },
   { key = 'l', mods = 'ALT', action = act.ShowLauncher },
   {
      key = 'o',
      mods = 'CTRL',
      action = act.ActivateKeyTable {
         name = "ctrl_o",
         timeout_milliseconds = 2000,
      },
   },
}

config.key_tables = {
   ctrl_o = {
      { key = 'o',
        action = act.SendKey {
           key = 'o',
           mods = 'CTRL',
        },
      },
      { key = 'o',
        mods = 'CTRL',
        action = act.ActivateLastTab,
      },
      { key = 'n', action = wezterm.action.ActivateTabRelative(1) },
      { key = 'p', action = wezterm.action.ActivateTabRelative(-1) },
      { key = 'c', action = act.SpawnTab 'CurrentPaneDomain' },
      { key = '-', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
      { key = '|', mods = 'SHIFT', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
      { key = 'LeftArrow', action = act.ActivatePaneDirection "Left" },
      { key = 'RightArrow', action = act.ActivatePaneDirection "Right" },
      { key = 'DownArrow', action = act.ActivatePaneDirection "Down" },
      { key = 'UpArrow', action = act.ActivatePaneDirection "Up" },
   },
}

for tab_no=1,9 do
   table.insert(config.key_tables.ctrl_o, { key = tostring(tab_no), action = act.ActivateTab(tab_no - 1) })
end


-- config.color_scheme = 'Zenburn'
config.colors = {
   background = "#000000",
   foreground = "#faebd7",
   ansi = {
      "#000000",
      "#cd0000",
      "#00cd00",
      "#cdcd00",
      "#0000cd",
      "#cd00cd",
      "#00cdcd",
      "#faebd7",
   },
   brights = {
      "#404040",
      "#ff0000",
      "#00ff00",
      "#ffff00",
      "#0000ff",
      "#ff00ff",
      "#00ffff",
      "#ffffff",
   }
}

config.window_frame = {
   font_size = 11,
   -- active_titlebar_bg = '#00ff00',
   -- inactive_titlebar_bg = '',
}

config.launch_menu = {
   {
      label = "MOSH:valak",
      args = {
         "bash", "--login", "-c", "mosh valak",
      },
   },
}

return config
