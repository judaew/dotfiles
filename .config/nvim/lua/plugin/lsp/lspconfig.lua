local lspconfig = require'lspconfig'
local cmp_nvim_lsp = require'cmp_nvim_lsp'

lspconfig.clangd.setup {
    capabilities = cmp_nvim_lsp.update_capabilities(vim.lsp.protocol.make_client_capabilities());
    cmd = { "clangd-mp-12", "--background-index" };
}
