local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

local lspconfig = (require'lspconfig')

lspconfig.pyright.setup {
    on_attach = on_attach,
    flags = lsp_flags,
}

lspconfig.rust_analyzer.setup {
  settings = {
    ['rust-analyzer'] = {},
  },
}

lspconfig.clangd.setup {

}

require'lspconfig'


