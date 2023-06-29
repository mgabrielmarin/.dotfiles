-- This file can be loaded by calling 'lua require('plugins')' from
-- init.vim

-- Only required if you have packer configured as 'opt'
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'
    use 'Mofiqul/dracula.nvim'
    -- use 'neovim/nvim-lspconfig'
    -- use {'neoclide/coc.nvim', branch = 'release' }
    use ({'nvim-treesitter/nvim-treesitter', run = ":TSUpdate"})
    --[===[
      run = function ()
        local ts_update = require('nvim-treesitter.install')
          .update({ with_sync = true })
        ts_update()
      end,
    }
    --]===]
    use {
      'VonHeikemen/lsp-zero.nvim',
      branch = 'v2.x',
      requires = {
        -- LSP Support
        {'neovim/nvim-lspconfig'},             -- Required
        {'williamboman/mason.nvim',
          run = function()
            pcall(vim.cmd, 'MasonUpdate')
          end,
        },
        {'williamboman/mason-lspconfig.nvim'}, -- Optional

        -- Autocompletion
        {'hrsh7th/nvim-cmp'},     -- Required
        {'hrsh7th/cmp-nvim-lsp'}, -- Required
        {'L3MON4D3/LuaSnip'},     -- Required
      }
    }

    use("github/copilot.vim")



end)
