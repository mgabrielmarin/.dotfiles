local lsp = require("lsp-zero")

lsp.preset("recommended")

lsp.ensure_installed({
  'rust_analyzer',
})

lsp.on_attach(
  function(client, bufnr)
  lsp.default_keymaps(
    {buffer = bufnr}
  )
end)

lsp.setup()

--[===[
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
--]===]
