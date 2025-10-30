-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- For example, changing the initial geometry for new windows:
config.initial_cols = 80
config.initial_rows = 25

-- or, changing the font size and color scheme.
config.font_size = 20
config.font = wezterm.font "Google Sans Code"
config.color_scheme = "Apple System Colors"

-- Finally, return the configuration to wezterm:
return config
