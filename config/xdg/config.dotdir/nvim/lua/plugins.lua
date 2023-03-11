return require('packer').startup({
    function(use)
        use {
            "rgroli/other.nvim",
            config = function()
                require("other-nvim").setup({
                    mappings = {
                        {
                            pattern = "/(.*).spec.tsx",
                            target = "/%1.tsx",
                            context = "implementation"
                        }, {
                            pattern = "/(.*).spec.tsx",
                            target = "/%1.component.tsx",
                            context = "implementation"
                        }, {
                            pattern = "/(.*).spec.tsx",
                            target = "/%1.page.tsx",
                            context = "implementation"
                        },
                        {
                            pattern = "/(.*).spec.ts",
                            target = "/%1.ts",
                            context = "implementation"
                        },
                        {
                            pattern = "/(.*).component.tsx",
                            target = "/%1.spec.tsx",
                            context = "test"
                        },
                        {
                            pattern = "/(.*).page.tsx",
                            target = "/%1.spec.tsx",
                            context = "test"
                        },
                        {
                            pattern = "/(.*).ts",
                            target = "/%1.spec.ts",
                            context = "test"
                        },
                        {
                            pattern = "/(.*).tsx",
                            target = "/%1.spec.tsx",
                            context = "test"
                        }
                    }
                })
            end
        }

        use {'kevinhwang91/nvim-bqf', ft = 'qf'}
        use {"lewis6991/impatient.nvim"}
        use {
            "ziontee113/syntax-tree-surfer",
            config = function()

                require("syntax-tree-surfer").setup()
                ---- Syntax Tree Surfer
                local opts = {noremap = true, silent = true}

                -- Normal Mode Swapping:
                -- Swap The Master Node relative to the cursor with it's siblings, Dot Repeatable
                vim.keymap.set("n", "<C-q>", function()
                    vim.opt.opfunc = "v:lua.STSSwapUpNormal_Dot"
                    return "g@l"
                end, {silent = true, expr = true})

                vim.keymap.set("n", "<C-w>", function()
                    vim.opt.opfunc = "v:lua.STSSwapDownNormal_Dot"
                    return "g@l"
                end, {silent = true, expr = true})

                ---- Swap Current Node at the Cursor with it's siblings, Dot Repeatable
                vim.keymap.set("n", "<C-.>", function()
                    vim.opt.opfunc = "v:lua.STSSwapCurrentNodeNextNormal_Dot"
                    return "g@l"
                end, {silent = true, expr = true})
                vim.keymap.set("n", "<C-,>", function()
                    vim.opt.opfunc = "v:lua.STSSwapCurrentNodePrevNormal_Dot"
                    return "g@l"
                end, {silent = true, expr = true})

                ----> If the mappings above don't work, use these instead (no dot repeatable)
                ---- vim.keymap.set("n", "vd", '<cmd>STSSwapCurrentNodeNextNormal<cr>', opts)
                ---- vim.keymap.set("n", "vu", '<cmd>STSSwapCurrentNodePrevNormal<cr>', opts)
                ---- vim.keymap.set("n", "vD", '<cmd>STSSwapDownNormal<cr>', opts)
                ---- vim.keymap.set("n", "vU", '<cmd>STSSwapUpNormal<cr>', opts)

                ---- Visual Selection from Normal Mode
                -- vim.keymap.set("n", "vx", '<cmd>STSSelectMasterNode<cr>', opts)
                -- vim.keymap.set("n", "vn", '<cmd>STSSelectCurrentNode<cr>', opts)

                ---- Select Nodes in Visual Mode
                -- vim.keymap.set("x", "J", '<cmd>STSSelectNextSiblingNode<cr>', opts)
                -- vim.keymap.set("x", "K", '<cmd>STSSelectPrevSiblingNode<cr>', opts)
                -- vim.keymap.set("x", "H", '<cmd>STSSelectParentNode<cr>', opts)
                -- vim.keymap.set("x", "L", '<cmd>STSSelectChildNode<cr>', opts)

                ---- Swapping Nodes in Visual Mode
                -- vim.keymap.set("x", "<A-j>", '<cmd>STSSwapNextVisual<cr>', opts)
                -- vim.keymap.set("x", "<A-k>", '<cmd>STSSwapPrevVisual<cr>', opts)
            end

        }
        -- use "creativenull/efmls-configs-nvim"
        use {
            "rcarriga/nvim-notify",
            config = function()
                require("notify").setup {renderer = "simple"}
                vim.notify = require('notify')
            end
        }

        use {
            'ojroques/nvim-lspfuzzy',
            requires = {
                {'junegunn/fzf'}, {'junegunn/fzf.vim'} -- to enable preview (optional)
            },

            config = function()

                require('lspfuzzy').setup {
                    methods = 'all', -- either 'all' or a list of LSP methods (see below)
                    jump_one = true, -- jump immediately if there is only one location
                    save_last = false, -- save last location results for the :LspFuzzyLast command
                    callback = nil -- callback called after jumping to a location
                    -- fzf_preview = { -- arguments to the FZF '--preview-window' option
                    --   'right:+{2}-/2' -- preview on the right and centered on entry
                    -- },
                    -- fzf_action = { -- FZF actions
                    --   ['ctrl-t'] = 'tab split', -- go to location in a new tab
                    --   ['ctrl-v'] = 'vsplit', -- go to location in a vertical split
                    --   ['ctrl-x'] = 'split', -- go to location in a horizontal split
                    -- },
                    -- fzf_modifier = ':~:.', -- format FZF entries, see |filename-modifiers|
                    -- fzf_trim = true, -- trim FZF entries
                }
            end
        }
        -- use {
        --   "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
        --   config = function()
        --     require("lsp_lines").setup()
        --     vim.diagnostic.config(
        --         {
        --           virtual_text = false
        --         }
        --     )
        --   end
        -- }

        -- use {
        --   'Equilibris/nx.nvim',
        --   requires = {
        --     'nvim-telescope/telescope.nvim'
        --   },
        --   config = function()
        --     require("nx").setup {
        --       nx_cmd_root = 'yarn nx'
        --     }
        --   end
        -- }

        use "b0o/SchemaStore.nvim"

        use {"knubie/vim-kitty-navigator", run = "cp ./*.py ~/.config/kitty/"}

        use {
            'rcarriga/neotest',
            requires = {'haydenmeade/neotest-jest'},
            module = "neotest",
            config = function()
                require('neotest').setup({
                    adapters = {
                        require('neotest-jest')({
                            jestCommand = "yarn jest",
                            env = {CI = true},
                            cwd = function(path)
                                return vim.fn.getcwd()
                            end
                        })
                    }
                })
            end
        }

        use {
            "EdenEast/nightfox.nvim",
            config = function() vim.cmd("colorscheme nightfox") end
        }
        use {
            'j-hui/fidget.nvim',
            config = function() require"fidget".setup() end
        }

        use 'doums/lsp_spinner.nvim'
        use "folke/lua-dev.nvim"
        use "euclidianAce/BetterLua.vim"
        use "kyazdani42/nvim-web-devicons"
        use "onsails/lspkind-nvim"
        use "junegunn/fzf.vim"
        use "junegunn/fzf"
        use "gfanto/fzf-lsp.nvim"
        use "tpope/vim-rhubarb"
        use "tpope/vim-fugitive"
        use "tpope/vim-surround"
        use "tpope/vim-dispatch"
        use "tpope/vim-eunuch"
        use "tpope/vim-commentary"
        -- use "peitalin/vim-jsx-typescript"
        -- use "mlaursen/vim-react-snippets"
        -- use "Pocco81/DAPInstall.nvim"
        -- use "jparise/vim-graphql"
        use "kshenoy/vim-signature"
        -- use "wellle/targets.vim"
        use "gioele/vim-autoswap"
        use "ryanoasis/vim-devicons"
        -- use "chrisbra/unicode.vim"
        -- use "bkad/CamelCaseMotion"
        -- use "PeterRincker/vim-argumentative"
        -- use "michaeljsmith/vim-indent-object"
        use {"ray-x/lsp_signature.nvim"}

        use {
            "gelguy/wilder.nvim",
            config = function()
                vim.api.nvim_exec([[
        call wilder#setup({'modes': [':', '/', '?']})
        call wilder#set_option('renderer', wilder#popupmenu_renderer())
        ]], false)
            end
        }

        use {"weilbith/nvim-code-action-menu", cmd = 'CodeActionMenu'}

        -- use {
        --   'mg979/vim-visual-multi',
        --   branch = "master"
        -- }

        -- use {
        --   "vuki656/package-info.nvim",
        --   requires = "MunifTanjim/nui.nvim"
        -- }

        -- use {
        --   'folke/lsp-colors.nvim',
        --   config = function()
        --     require("lsp-colors").setup(
        --         {
        --           Error = "#db4b4b",
        --           Warning = "#e0af68",
        --           Information = "#0db9d7",
        --           Hint = "#10B981"
        --         }
        --     )
        --   end
        -- }

        -- use {
        --   'nvim-telescope/telescope.nvim',
        --   requires = {
        --     {
        --       'nvim-lua/plenary.nvim'
        --     }
        --   }
        -- }

        -- use {
        --   'glepnir/dashboard-nvim',
        --   requires = "liuchengxu/vim-clap"
        -- }
        --
        -- use {
        --   "p00f/nvim-ts-rainbow",
        --   config = function()
        --     require'nvim-treesitter.configs'.setup {
        --       rainbow = {
        --         enable = true,
        --         extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
        --         max_file_lines = 1000 -- Do not enable for files with more than 1000 lines, int
        --       }
        --     }
        --   end
        -- }

        -- use {
        --   'mfussenegger/nvim-dap',
        --   config = function()
        --     require("nvim-dap.init")
        --   end
        -- }

        -- use {
        --   "rcarriga/nvim-dap-ui",
        --   requires = "mfussenegger/nvim-dap",
        --   config = function()
        --     require("dapui").setup {
        --       icons = {
        --         expanded = "▾",
        --         collapsed = "▸"
        --       },
        --       mappings = {
        --         -- Use a table to apply multiple mappings
        --         expand = {
        --           "<CR>",
        --           "<2-LeftMouse>"
        --         },
        --         open = "o",
        --         remove = "d",
        --         edit = "e"
        --       },
        --       floating = {
        --         max_height = nil, -- These can be integers or a float between 0 and 1.
        --         max_width = nil -- Floats will be treated as percentage of your screen.
        --       }
        --     }
        --   end
        -- }

        -- -- Profile startuptime
        use {'tweekmonster/startuptime.vim', command = "StartupTime"}

        use {
            "lukas-reineke/lsp-format.nvim",
            config = function() require('lsp-format').setup {} end
        }

        -- use {
        --   'honza/vim-snippets',
        --   rtp = '.'
        -- }

        -- use {
        --   'sindrets/diffview.nvim',
        --   cmd = "DiffviewOpen"
        -- }

        use {
            'windwp/nvim-autopairs',
            config = function() require('nvim-autopairs').setup() end
        }

        -- use {
        --   'vim-test/vim-test',
        --   config = function()
        --   end
        -- }

        use {
            'kyazdani42/nvim-tree.lua',
            requires = {
                'kyazdani42/nvim-web-devicons' -- optional, for file icons
            },
            cmd = "NvimTreeFindFileToggle",
            config = function()
                require('nvim-tree').setup({
                    sync_root_with_cwd = true,
                    update_focused_file = {enable = true, update_root = true},
                    diagnostics = {enable = true, show_on_dirs = true},
                    view = {
                        mappings = {list = {{key = "<C-e>", action = ""}}},
                        adaptive_size = true
                    }
                })
            end,
            tag = 'nightly' -- optional, updated every week. (see issue #1193)
        }

        -- use {
        --   "nvim-neo-tree/neo-tree.nvim",
        --   branch = "v1.x",
        --   requires = {
        --     "nvim-lua/plenary.nvim",
        --     "kyazdani42/nvim-web-devicons", -- not strictly required, but recommended
        --     "MunifTanjim/nui.nvim"
        --   },
        --   config = function()
        --     -- See ":help neo-tree-highlights" for a list of available highlight groups
        --     vim.cmd(
        --         [[
        -- hi link NeoTreeDirectoryName Directory
        -- hi link NeoTreeDirectoryIcon NeoTreeDirectoryName
        -- ]]
        --     )

        --     require("neo-tree").setup(
        --         {
        --           close_if_last_window = false, -- Close Neo-tree if it is the last window left in the tab
        --           popup_border_style = "rounded",
        --           enable_git_status = true,
        --           enable_diagnostics = true,
        --           default_component_configs = {
        --             indent = {
        --               indent_size = 2,
        --               padding = 1, -- extra padding on left hand side
        --               with_markers = true,
        --               indent_marker = "│",
        --               last_indent_marker = "└",
        --               highlight = "NeoTreeIndentMarker"
        --             },
        --             icon = {
        --               folder_closed = "",
        --               folder_open = "",
        --               folder_empty = "ﰊ",
        --               default = "*"
        --             },
        --             name = {
        --               trailing_slash = false,
        --               use_git_status_colors = true
        --             },
        --             git_status = {
        --               highlight = "NeoTreeDimText" -- if you remove this the status will be colorful
        --             }
        --           },
        --           filesystem = {
        --             filters = { -- These filters are applied to both browsing and searching
        --               show_hidden = true,
        --               respect_gitignore = true
        --             },
        --             follow_current_file = true, -- This will find and focus the file in the active buffer every
        --             -- time the current file is changed while the tree is open.
        --             use_libuv_file_watcher = false, -- This will use the OS level file watchers
        --             -- to detect changes instead of relying on nvim autocmd events.
        --             hijack_netrw_behavior = "open_default", -- netrw disabled, opening a directory opens neo-tree
        --             -- in whatever position is specified in window.position
        --             -- "open_split",  -- netrw disabled, opening a directory opens within the
        --             -- window like netrw would, regardless of window.position
        --             -- "disabled",    -- netrw left alone, neo-tree does not handle opening dirs
        --             window = {
        --               position = "left",
        --               width = 40,
        --               mappings = {
        --                 ["<2-LeftMouse>"] = "open",
        --                 ["<cr>"] = "open",
        --                 ["S"] = "open_split",
        --                 ["s"] = "open_vsplit",
        --                 ["C"] = "close_node",
        --                 ["<bs>"] = "navigate_up",
        --                 ["."] = "set_root",
        --                 ["H"] = "toggle_hidden",
        --                 ["I"] = "toggle_gitignore",
        --                 ["R"] = "refresh",
        --                 ["/"] = "fuzzy_finder",
        --                 -- ["/"] = "filter_as_you_type", -- this was the default until v1.28
        --                 -- ["/"] = "none" -- Assigning a key to "none" will remove the default mapping
        --                 ["f"] = "filter_on_submit",
        --                 ["<c-x>"] = "clear_filter",
        --                 ["a"] = "add",
        --                 ["d"] = "delete",
        --                 ["r"] = "rename",
        --                 ["c"] = "copy_to_clipboard",
        --                 ["x"] = "cut_to_clipboard",
        --                 ["p"] = "paste_from_clipboard",
        --                 ["m"] = "move", -- takes text input for destination
        --                 ["q"] = "close_window"
        --               }
        --             }
        --           },
        --           buffers = {
        --             show_unloaded = true,
        --             window = {
        --               position = "left",
        --               mappings = {
        --                 ["<2-LeftMouse>"] = "open",
        --                 ["<cr>"] = "open",
        --                 ["S"] = "open_split",
        --                 ["s"] = "open_vsplit",
        --                 ["<bs>"] = "navigate_up",
        --                 ["."] = "set_root",
        --                 ["R"] = "refresh",
        --                 ["a"] = "add",
        --                 ["d"] = "delete",
        --                 ["r"] = "rename",
        --                 ["c"] = "copy_to_clipboard",
        --                 ["x"] = "cut_to_clipboard",
        --                 ["p"] = "paste_from_clipboard",
        --                 ["bd"] = "buffer_delete"
        --               }
        --             }
        --           },
        --           git_status = {
        --             window = {
        --               position = "float",
        --               mappings = {
        --                 ["<2-LeftMouse>"] = "open",
        --                 ["<cr>"] = "open",
        --                 ["S"] = "open_split",
        --                 ["s"] = "open_vsplit",
        --                 ["C"] = "close_node",
        --                 ["R"] = "refresh",
        --                 ["d"] = "delete",
        --                 ["r"] = "rename",
        --                 ["c"] = "copy_to_clipboard",
        --                 ["x"] = "cut_to_clipboard",
        --                 ["p"] = "paste_from_clipboard",
        --                 ["A"] = "git_add_all",
        --                 ["gu"] = "git_unstage_file",
        --                 ["ga"] = "git_add_file",
        --                 ["gr"] = "git_revert_file",
        --                 ["gc"] = "git_commit",
        --                 ["gp"] = "git_push",
        --                 ["gg"] = "git_commit_and_push"
        --               }
        --             }
        --           }
        --         }
        --     )
        --     vim.cmd([[nnoremap \ :NeoTreeReveal<cr>]])
        --   end
        -- }
        use {
            'hrsh7th/nvim-cmp',
            requires = {
                'quangnguyen30192/cmp-nvim-ultisnips', -- {
                --   'tzachar/cmp-tabnine',
                --   run = './install.sh'
                -- },
                'hrsh7th/cmp-buffer', 'hrsh7th/cmp-nvim-lsp',
                'hrsh7th/cmp-path', 'hrsh7th/cmp-nvim-lua',
                'onsails/lspkind-nvim',
                {'David-Kunz/cmp-npm', requires = 'nvim-lua/plenary.nvim'}

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
                            vim_item.kind =
                                lspkind.presets.default[vim_item.kind]
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
                        ['<S-Tab>'] = cmp.mapping(
                            cmp.mapping.select_prev_item(), {'i', 's'}),
                        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
                        ['<C-f>'] = cmp.mapping.scroll_docs(4),
                        ['<C-Space>'] = cmp.mapping.complete(),
                        ['<C-e>'] = cmp.mapping.close(),
                        ['<CR>'] = cmp.mapping.confirm({select = false})
                    },
                    sources = {
                        {name = 'nvim_lsp'}, {name = 'ultisnips'}, -- {
                        --   name = 'cmp_tabnine'
                        -- },
                        {name = 'nvim_lua'}, {name = 'buffer'}, {name = 'path'},
                        {name = 'npm', keyword_length = 4}
                    },
                    window = {
                        documentation = {
                            border = {
                                "┌", "─", "┐", "│", "┘", "─", "└",
                                "│"
                            }
                        }
                    }
                })
            end
        }

        use "kosayoda/nvim-lightbulb"

        use {
            "smjonas/inc-rename.nvim",
            config = function() require("inc_rename").setup() end
        }

        use {
            "williamboman/mason-lspconfig.nvim",
            requires = {"williamboman/mason.nvim", "hrsh7th/cmp-nvim-lsp"},
            config = function()
                require("mason").setup()
                require("mason-lspconfig").setup({
                    ensure_installed = {
                        "pylsp", "angularls", "awk_ls", "bashls", "cssls",
                        "cssmodules_ls", "dockerls", "emmet_ls", "eslint",
                        "grammarly", "html", "jsonls", "sqls", "sumneko_lua",
                        "tailwindcss", "tsserver", "yamlls", "vimls"
                    }
                })

                local on_attach = function(client, bufnr)
                    local lsp_spinner = require('lsp_spinner')
                    local capabilities = vim.lsp.protocol
                                             .make_client_capabilities()

                    -- turn on `window/workDoneProgress` capability
                    lsp_spinner.init_capabilities(capabilities)
                    local lsp_signature = require('lsp_signature')
                    local lsp_keymaps = require('lsp.keymaps')
                    local lsp_autocommands = require('lsp.autocommands')
                    local lsp_signs = require('lsp.signs')
                    local lsp_handlers = require('lsp.handlers')
                    lsp_spinner.on_attach(client, bufnr)
                    lsp_keymaps.init()
                    lsp_signature.on_attach({
                        bind = true,
                        handler_opts = {border = "rounded"}
                    }, bufnr)
                    lsp_autocommands.init(client)
                    lsp_signs.init()
                    lsp_handlers.init()
                end

                local capabilities =
                    require('cmp_nvim_lsp').default_capabilities()

                require("mason-lspconfig").setup_handlers {
                    function(server_name)
                        require("lspconfig")[server_name].setup {
                            on_attach = on_attach,
                            capabilities = capabilities
                        }
                    end,
                    ['sumneko_lua'] = function()
                        require("lspconfig")['sumneko_lua'].setup {
                            on_attach = on_attach,
                            capabilities = capabilities,
                            settings = {
                                Lua = {diagnostics = {globals = {'vim'}}}
                            }
                        }
                    end,

                    ['jsonls'] = function()
                        require("lspconfig")['jsonls'].setup {
                            on_attach = on_attach,
                            capabilities = capabilities,
                            init_options = {provideFormatter = false},
                            json = {
                                schemas = require('schemastore').json.schemas(),
                                validate = {enable = true}
                            }
                        }
                    end,

                    ["tsserver"] = function()
                        local react_filter = require("lsp.react-filter")
                        -- local util = require 'lspconfig.util'
                        require("lspconfig")['tsserver'].setup {
                            capabilities = capabilities,
                            -- root_dir = function(fname)
                            --     return util.root_pattern('.git')(fname)
                            -- end,
                            handlers = {
                                ['textDocument/definition'] = react_filter
                            },
                            debounce_text_changes = 150,
                            on_attach = function(client, bufnr)
                                local ts_utils = require("nvim-lsp-ts-utils")
                                ts_utils.setup {
                                    eslint_enable_diagnostics = false,
                                    disable_commands = false,
                                    -- eslint_enable_disable_comments = true,
                                    enable_formatting = false,
                                    update_imports_on_move = true,
                                    require_confirmation_on_move = false,
                                    watch_dir = nil
                                }

                                client.server_capabilities.document_formatting =
                                    false
                                ts_utils.setup_client(client)
                                on_attach(client, bufnr)
                            end
                        }
                    end
                }
            end
        }

        -- use 'nvim-lua/lsp_extensions.nvim'

        use {'RishabhRD/nvim-lsputils', requires = 'RishabhRD/popfix'}

        use "neovim/nvim-lspconfig"

        use {
            "jose-elias-alvarez/null-ls.nvim",
            config = function()
                local null_ls = require("null-ls")

                null_ls.setup({
                    sources = {
                        null_ls.builtins.formatting.prettier,
                        null_ls.builtins.formatting.lua_format,
                        null_ls.builtins.formatting.mdformat,
                        null_ls.builtins.formatting.stylelint,
                        null_ls.builtins.formatting.yamlfmt,
                        null_ls.builtins.formatting.fixjson
                    },
                    on_attach = function(client)
                        require"lsp-format".on_attach(client)
                    end
                })
            end
        }

        use {
            'jose-elias-alvarez/nvim-lsp-ts-utils',
            requires = {"benwainwright/null-ls.nvim"}
        }

        use {
            "SirVer/ultisnips",
            config = function()
                vim.api.nvim_set_var("UltiSnipsExpandTrigger", "<NUL>")
            end
        }

        -- use {
        --   "folke/trouble.nvim",
        --   requires = "kyazdani42/nvim-web-devicons",
        --   cmd = {
        --     "Trouble",
        --     "LspTrouble"
        --   },
        --   config = function()
        --     require("trouble").setup()
        --   end
        -- }

        -- use {
        --   "weirongxu/plantuml-previewer.vim",
        --   ft = "plantuml",
        --   requires = {
        --     "tyru/open-browser.vim",
        --     "aklt/plantuml-syntax"
        --   }
        -- }
        --

        use {
            'nvim-treesitter/nvim-treesitter',
            run = ':TSUpdate',
            config = function()
                local ts = require 'nvim-treesitter.configs'
                ts.setup {
                    ensure_installed = {
                        'javascript', 'typescript', 'bash', 'css', 'help',
                        'html', 'jsdoc', 'json', 'json5', 'jsonc', 'lua',
                        'make', 'python', 'regex', 'scss', 'svelte', 'tsx',
                        'vim', 'yaml'
                    },
                    highlight = {enable = true}
                }
            end
        }

        use {
            'benwainwright/fzf-project'
            -- '~/repos/fzf-project'
        }

        -- use {
        --   'TimUntersberger/neogit',
        --   cmd = "Neogit",
        --   config = function()
        --     require("neogit").setup()
        --   end
        -- }

        -- use {
        --   "lewis6991/gitsigns.nvim",
        --   config = function()
        --     require("gitsigns").setup()
        --   end
        -- }

        -- use {
        --   "lbrayner/vim-rzip",
        --   config = function()
        --     vim.cmd(
        --         [[
        -- " See https://yarnpkg.com/getting-started/editor-sdks#vim
        -- " Decode URI encoded characters
        -- function! DecodeURI(uri)
        --   return substitute(a:uri, '%\([a-fA-F0-9][a-fA-F0-9]\)', '\=nr2char("0x" . submatch(1))', "g")
        -- endfunction

        -- " Attempt to clear non-focused buffers with matching name
        -- function! ClearDuplicateBuffers(uri)
        --   " if our filename has URI encoded characters
        --   if DecodeURI(a:uri) !=# a:uri
        --       " wipeout buffer with URI decoded name - can print error if buffer in focus
        --       sil! exe "bwipeout " . fnameescape(DecodeURI(a:uri))
        --       " change the name of the current buffer to the URI decoded name
        --       exe "keepalt file " . fnameescape(DecodeURI(a:uri))
        --       " ensure we don't have any open buffer matching non-URI decoded name
        --       sil! exe "bwipeout " . fnameescape(a:uri)
        --   endif
        -- endfunction

        -- function! RzipOverride()
        --   " Disable vim-rzip's autocommands
        --   autocmd! zip BufReadCmd   zipfile:*,zipfile:*/*
        --   exe "au! zip BufReadCmd ".g:zipPlugin_ext

        --   " order is important here, setup name of new buffer correctly then fallback to vim-rzip's handling
        --   autocmd zip BufReadCmd   zipfile:*  call ClearDuplicateBuffers(expand("<amatch>"))
        --   autocmd zip BufReadCmd   zipfile:*  call rzip#Read(DecodeURI(expand("<amatch>")), 1)

        --   if has("unix")
        --       autocmd zip BufReadCmd   zipfile:*/*  call ClearDuplicateBuffers(expand("<amatch>"))
        --       autocmd zip BufReadCmd   zipfile:*/*  call rzip#Read(DecodeURI(expand("<amatch>")), 1)
        --   endif

        --   exe "au zip BufReadCmd ".g:zipPlugin_ext."  call rzip#Browse(DecodeURI(expand('<amatch>')))"
        -- endfunction

        -- autocmd VimEnter * call RzipOverride()
        -- ]]
        --     )
        --   end
        -- }

        -- use {
        --   "hoob3rt/lualine.nvim",
        --   config = function()
        --     require("lualine").setup {
        --       options = {
        --         section_separators = {
        --           "",
        --           ""
        --         },
        --         component_separators = {
        --           "",
        --           ""
        --         }
        --       }
        --     }
        --   end
        -- }

    end,

    config = {max_jobs = 50}
})
