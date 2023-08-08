local cmd = vim.cmd
local dracula = require("dracula")

dracula.setup({
  -- customize dracula color palette
  colors = {
    bg = "#000000",
    comment = "#008000"
  }
})

vim.cmd('colorscheme dracula')
-- vim.cmd('set background=dark')
-- vim.cmd('highlight Comment ctermfg=green guifg=green')

