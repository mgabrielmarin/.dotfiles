--[==[
local dracula = require("dracula")

dracula.setup({
  -- customize dracula color palette
  colors = {
    bg = "#000000",
    comment = "#008000"
  }
})

vim.cmd('colorscheme dracula')

]==]

local catppuccin = require("catppuccin")

catppuccin.setup({
    color_overrides = {
        all = {
            base = "#000000",
        },
    }
})

vim.cmd.colorscheme "catppuccin"
