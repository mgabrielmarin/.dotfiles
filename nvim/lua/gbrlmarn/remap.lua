vim.g.mapleader = " "

vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)

vim.keymap.set(
    "n", "<leader>vpp",
    "<cmd>e ~/fun/repos/.dotfiles/nvim/lua/gbrlmarn/packer.lua<CR>"
);

vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so")
end)


