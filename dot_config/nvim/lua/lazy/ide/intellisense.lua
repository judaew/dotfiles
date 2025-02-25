-- luacheck: push ignore entry
-- luacheck: push ignore vim_item
local function complete_format(entry, vim_item)
  -- Use nerd font
  local kind_icons = {
    Text          = " ",
    Method        = "󰆧 ",
    Function      = "󰊕 ",
    Constructor   = " ",
    Field         = "󰇽 ",
    Variable      = "󰂡 ",
    Class         = "󰠱 ",
    Interface     = " ",
    Module        = " ",
    Property      = "󰜢 ",
    Unit          = " ",
    Value         = "󰎠 ",
    Enum          = " ",
    Keyword       = "󰌋 ",
    Snippet       = " ",
    Color         = "󰏘 ",
    File          = "󰈙 ",
    Reference     = " ",
    Folder        = "󰉋 ",
    EnumMember    = " ",
    Constant      = "󰏿 ",
    Struct        = " ",
    Event         = " ",
    Operator      = "󰆕 ",
    TypeParameter = "󰅲 ",
  }
  local kind_names = {
    Text = "Txt",
    Method = "Mthd",
    Function = "Func",
    Constructor = "Ctor",
    Field = "Fld",
    Variable = "Var",
    Class = "Cls",
    Interface = "Intf",
    Module = "Mod",
    Property = "Prop",
    Unit = "Unit",
    Value = "Val",
    Enum = "Enum",
    Keyword = "Kw",
    Snippet = "Snip",
    Color = "Clr",
    File = "File",
    Reference = "Ref",
    Folder = "Fldr",
    EnumMember = "EnumMbr",
    Constant = "Const",
    Struct = "Struct",
    Event = "Evt",
    Operator = "Op",
    TypeParameter = "TypeParam",
  }

  local menu_names = {
    nvim_lsp = "LSP",
    ctags    = "TAG",
    vsnip    = "SNIP",
    buffer   = "BUF",
    tmux     = "TMUX",
    path     = "PATH",
    cmdline  = "CMD",
    nvim_lua = "VIM",
    git      = "GIT",
  }

  return function(entry, vim_item)
    local maxLength = 40
    vim_item.abbr = string.sub(vim_item.abbr, 1, maxLength)
    vim_item.kind = string.format("%s%s", kind_icons[vim_item.kind], kind_names[vim_item.kind])
    vim_item.menu = string.format("%s", menu_names[entry.source.name])
    return vim_item
  end
end
-- luacheck: pop
-- luacheck: pop

return {
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    keys = { ":", "/", "?" },
    dependencies = {
      -- autopairing of (){}[] etc
      -- TODO: see: "altermo/ultimate-autopair.nvim",
      {
        "windwp/nvim-autopairs",
        -- The upstream is unstable
        commit = "eac31b4797ce4fa9dd546f7b98ec32305527b19e",
        event = "InsertEnter",
        config = function()
          require("nvim-autopairs").setup({
            fast_wrap = {},
            disable_filetype = { "TelescopePrompt", "vim" },
          })

          -- setup cmp for autopairs
          local cmp_autopairs = require("nvim-autopairs.completion.cmp")
          require("cmp").event:on("confirm_done", cmp_autopairs.on_confirm_done())
        end,
      },
      -- cmp sources plugins
      {
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-cmdline",
        "hrsh7th/cmp-nvim-lua",
        "andersevenrud/cmp-tmux"
      },
      -- snippet plugin
      {
        "hrsh7th/vim-vsnip",
        dependencies = {
          "hrsh7th/cmp-vsnip"
        },
        config = function()
          vim.g.vsnip_snippet_dir = os.getenv("HOME") .. "/.local/share/nvim/vsnip"

          vim.cmd([[
                    " Expand
                    imap <expr> <C-j>   vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<C-j>'
                    smap <expr> <C-j>   vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<C-j>'

                    " Expand or jump
                    imap <expr> <C-l>   vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
                    smap <expr> <C-l>   vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

                    " Jump forward or backward
                    imap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'
                    smap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<Tab>'
                    imap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
                    smap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
                    ]])
        end
      },
      -- git
      { "petertriho/cmp-git" }
      -- TODO: "paopaol/cmp-doxygen"
    },
    config = function()
      local cmp = require("cmp")

      cmp.setup({
        snippet = {
          expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)             -- For `vsnip` user.
          end,
        },
        window = {
          completion = {
            scrollbar = true,
          },
          documentation = {
            max_height = 20
          }
        },
        experimental = {
          ghost_text = true
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-n>"] = cmp.mapping.select_next_item(),
          ["<C-p>"] = cmp.mapping.select_prev_item(),
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-e>"] = cmp.mapping.close(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }),
        }),
        sources = {
          { name = "nvim_lsp", priority = 1 },
          { name = "vsnip",    priority = 1 },
          { name = "buffer",   priority = 2 },
          { name = "tmux",     priority = 2, option = { all_panes = true } },
          { name = "nvim_lua", priority = 1 },
        },
        -- luacheck: push ignore entry
        -- luacheck: push ignore vim_item
        formatting = {
          format = complete_format(entry, vim_item)
        }
        -- luacheck: pop
        -- luacheck: pop
      })

      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" }
        }
      })

      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "cmdline" },
          { name = "path" }
        }
      })

      cmp.setup.filetype("gitcommit", {
        sources = {
          { name = "git" },
          { name = "buffer" },
          { name = "tmux" }
        }
      })
      require("cmp_git").setup()

      cmp.setup.filetype("org", {
        sources = {
          { name = "vsnip" },
          { name = "orgmode" },
          { name = "buffer" },
          { name = "tmux",   option = { all_panes = true } },
        }
      })
    end
  }
}
