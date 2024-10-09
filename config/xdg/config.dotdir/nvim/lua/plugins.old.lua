return require('packer').startup({
    function(use)

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

        -- use "wellle/targets.vim"
        -- use "chrisbra/unicode.vim"
        -- use "bkad/CamelCaseMotion"
        -- use "PeterRincker/vim-argumentative"
        -- use "michaeljsmith/vim-indent-object"

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
        --   'nvim-telescope/telescope.nvim',
        --   requires = {
        --     {
        --       'nvim-lua/plenary.nvim'
        --     }
        --   }
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

        -- use {
        --   'honza/vim-snippets',
        --   rtp = '.'
        -- }

        -- use {
        --   'sindrets/diffview.nvim',
        --   cmd = "DiffviewOpen"
        -- }


        use {
            "smjonas/inc-rename.nvim",
            config = function() require("inc_rename").setup() end
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
        --

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

    end,

    config = {max_jobs = 50, ensure_dependencies = true}
})
