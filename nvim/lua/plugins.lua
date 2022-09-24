-- This file can be loaded by calling 'lua require('plugins')' from
-- init.vim

-- Only required if you have packer configured as 'opt'
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
	-- Packer can manage itself
	use 'wbthomason/packer.nvim'
	use 'Olical/conjure'
	use 'Olical/aniseed'
  use 'Mofiqul/dracula.nvim'
end)
