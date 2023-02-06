local lspconfig = require'lspconfig'
-- See https://github.com/hrsh7th/cmp-nvim-lsp/tree/59224771f91b86d1de12570b4070fe4ad7cd1eeb#capabilities
local compose_capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

lspconfig.clangd.setup {
    capabilities = compose_capabilities,
    cmd = { vim.fn.exepath('clangd-mp-15'),
            '--all-scopes-completion',
            '--suggest-missing-includes',
            '--background-index',
            '--cross-file-rename',
            '--log=info',
            '--completion-style=detailed',
            '--clang-tidy',
            '--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type',
            '--fallback-style=Google',
            '--header-insertion=never',

            -- clangd 11+ supports reading from .clangd configuration file
            '--enable-config',

            -- store PCHs in RAM
            '--pch-storage=memory'}
}

lspconfig.gopls.setup {}

lspconfig.sumneko_lua.setup {
    capabilities = compose_capabilities,
    cmd = {vim.fn.exepath('lua-language-server')};
    settings = {
        Lua = {
            runtime = {
                version = 'LuaJIT',
                path = vim.split(package.path, ';')
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = {'vim', 'use'},
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = {[vim.fn.expand('$VIMRUNTIME/lua')] = true}
            },
            telemetry = {
                enable = false,
            }
        }
    }
}

lspconfig.pyright.setup {
    capabilities = compose_capabilities,
    cmd = {vim.fn.exepath('pyright-langserver'), "--pythonversion 3.10", "--stdio",}
}
