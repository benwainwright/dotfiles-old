return {
    'hrsh7th/nvim-cmp',
    event = 'VeryLazy',
    dependencies = {
        'quangnguyen30192/cmp-nvim-ultisnips', 'hrsh7th/cmp-buffer',
        'hrsh7th/cmp-nvim-lsp', 'hrsh7th/cmp-path', 'hrsh7th/cmp-nvim-lua',
        'onsails/lspkind-nvim',
        {'David-Kunz/cmp-npm', dependencies = 'nvim-lua/plenary.nvim'}

    },
    config = function()
        local lspkind = require 'lspkind'

        lspkind.init({
            mode = 'symbol',
            preset = 'codicons',
            symbol_map = {
                Text = '',
                Method = 'ƒ',
                Function = '',
                Constructor = '',
                Variable = '',
                Class = '',
                Interface = 'ﰮ',
                Module = '',
                Property = '',
                Unit = '',
                Value = '',
                Enum = '了',
                Keyword = '',
                Snippet = '﬌',
                Color = '',
                File = '',
                Folder = '',
                EnumMember = '',
                Constant = '',
                Struct = ''
            }
        })
        local cmp = require 'cmp'

        cmp.setup({
            formatting = {
                format = function(entry, vim_item)
                    vim_item.kind = lspkind.presets.default[vim_item.kind]
                    return vim_item
                end
            },

            snippet = {
                expand = function(args)
                    vim.fn["UltiSnips#Anon"](args.body)
                end
            },
            mapping = {
                ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(),
                                        {'i', 's'}),
                ['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(),
                                          {'i', 's'}),
                ['<C-d>'] = cmp.mapping.scroll_docs(-4),
                ['<C-f>'] = cmp.mapping.scroll_docs(4),
                ['<C-Space>'] = cmp.mapping.complete(),
                ['<C-e>'] = cmp.mapping.close(),
                ['<CR>'] = cmp.mapping.confirm({select = false})
            },
            sources = {
                {
                    name = "lazydev",
                    group_index = 0 -- set group index to 0 to skip loading LuaLS completions
                }, {name = 'nvim_lsp'}, {name = 'ultisnips'}, -- {
                --   name = 'cmp_tabnine'
                -- },
                {name = 'nvim_lua'}, {name = 'buffer'}, {name = 'path'},
                {name = 'npm', keyword_length = 4}
            },
            window = {
                documentation = {
                    border = {
                        "┌", "─", "┐", "│", "┘", "─", "└", "│"
                    }
                }
            }
        })
    end
}
