return require('packer').startup({ function(use)

  use {
    "gelguy/wilder.nvim",
    config = function()
      vim.api.nvim_exec(
      [[
        call wilder#setup({'modes': [':', '/', '?']})
        call wilder#set_option('renderer', wilder#popupmenu_renderer())
      ]],
      false
      )
    end
  }
  use "kyazdani42/nvim-web-devicons"
  use "onsails/lspkind-nvim"
  use "ynkdir/vim-vimlparser"
  use "junegunn/fzf.vim"
  use "junegunn/fzf"
  use "gfanto/fzf-lsp.nvim"
  use "tpope/vim-rhubarb"
  use "tpope/vim-fugitive"
  use "tpope/vim-surround"
  use "tpope/vim-dispatch"
  use "tpope/vim-eunuch"
  use "tpope/vim-commentary"
  use "fladson/vim-kitty"
  use "peitalin/vim-jsx-typescript"
  use "mlaursen/vim-react-snippets"
  use "christoomey/vim-tmux-navigator"
  use "David-Kunz/jester"
  use "Pocco81/DAPInstall.nvim"
  use "jparise/vim-graphql"
  use "mbbill/undotree"
  use "RishabhRD/popfix"
  use "nvim-lua/plenary.nvim"
  use "kshenoy/vim-signature"
  use "wellle/targets.vim"
  use "gioele/vim-autoswap"
  use "ryanoasis/vim-devicons"
  use "chrisbra/unicode.vim"
  use "bkad/CamelCaseMotion"
  use "PeterRincker/vim-argumentative"
  use "michaeljsmith/vim-indent-object"

  use {
    'CosmicNvim/cosmic-ui',
    requires = { 'MunifTanjim/nui.nvim', 'nvim-lua/plenary.nvim' },
    config = function()
      require('cosmic-ui').setup {}
    end,
  }


  use { 
    "williamboman/nvim-lsp-installer",
    requires = {
      "neovim/nvim-lspconfig",
    },
    config = function ()
      require("lsp.init")
    end
  }

  use {
    'mg979/vim-visual-multi',
    branch = "master"
  }

  use {
      "vuki656/package-info.nvim",
      requires = "MunifTanjim/nui.nvim",
  }

  use {
    'folke/lsp-colors.nvim',
    config = function()
      require("lsp-colors").setup({
        Error = "#db4b4b",
        Warning = "#e0af68",
        Information = "#0db9d7",
        Hint = "#10B981"
      })
    end
  }

  -- use {
  --   "ray-x/lsp_signature.nvim",

  --   config = function()
  --     require("lsp_signature").setup {
  --       floating_window = true
  --     }
  --   end
  -- }

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use {
    'glepnir/dashboard-nvim',
    requires="liuchengxu/vim-clap"
  }

  use {
    "p00f/nvim-ts-rainbow",
    config = function()
      require'nvim-treesitter.configs'.setup {
        rainbow = {
          enable = true,
          extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
          max_file_lines = 1000 -- Do not enable for files with more than 1000 lines, int
        }
      }
    end
  }

  use {
    'mfussenegger/nvim-dap',
    config = function ()
      require("configure-dap")
    end
  }

  use {
    "rcarriga/nvim-dap-ui",
    requires = "mfussenegger/nvim-dap",
    config = function()
      require("dapui").setup {
        icons = {
          expanded = "▾",
          collapsed = "▸"
        },
        mappings = {
          -- Use a table to apply multiple mappings
          expand = {"<CR>", "<2-LeftMouse>"},
          open = "o",
          remove = "d",
          edit = "e",
        },
        sidebar = {
          elements = {
            -- You can change the order of elements in the sidebar
            "scopes",
            "breakpoints",
            "stacks",
            "watches"
          },
          position = "left" -- Can be "left" or "right"
        },
        tray = {
          elements = {
            "repl"
          },
          position = "bottom" -- Can be "bottom" or "top"
        },
        floating = {
          max_height = nil, -- These can be integers or a float between 0 and 1.
          max_width = nil   -- Floats will be treated as percentage of your screen.
        }
      }
    end
  }
  
  -- Profile startuptime
  use {
    'tweekmonster/startuptime.vim',
    command = "StartupTime"
  }

  use {
    'honza/vim-snippets',
    rtp = '.'
  }

  use { 
    'sindrets/diffview.nvim',
    cmd = "DiffviewOpen"
  }

  use {
     'windwp/nvim-autopairs',
     config = function() require('nvim-autopairs').setup() end
  }

  use {
    'vim-test/vim-test',
    config = function()
      -- vim.api.nvim_exec(
      -- [=[
      --   nnoremap <leader>dt :TestNearest -strategy=jest<CR>
      --   function! JestStrategy(cmd)
      --     let testName = matchlist(a:cmd, '\v -t ''(.*)''')[1]
      --     let fileName = matchlist(a:cmd, '\v'' -- (.*)$')[1]
      --     call luaeval("require'debug-helper'.debugJest([[" . testName . "]], [[" . fileName . "]])")
      --   endfunction      
      --   let g:test#custom_strategies = {'jest': function('JestStrategy')}
      -- ]=], false)
    end
  }

  use {
    "rcarriga/vim-ultest",
    requires = {"vim-test/vim-test"},
    run = ":UpdateRemotePlugins",
    config = function()
      local nv = require("nvim-api")
      nv.map("n", "<leader>tn", "<cmd>UltestNearest<CR>")
      nv.map("n", "<leader>tf", "<cmd>Ultest<CR>")
    end
  }

  use {
    'kyazdani42/nvim-tree.lua',
    config = function()
      require'nvim-tree'.setup {
        open_on_setup = true,
        auto_close = true,
        update_focused_file = {
          enable = true,
          update_cwd = true
        },
        update_cwd = true,
        diagnostics = {
          enable = true,
          show_on_dirs = true
        },
        git = {
          enable = true,
        },
        view = {
          width = 50,
          auto_resize = true
        },
      }
      
      vim.api.nvim_set_var("nvim_tree_highlight_opened_files", 1)
      vim.api.nvim_set_var("nvim_tree_git_hl", 1)
      vim.api.nvim_set_var("nvim_tree_showicons.git", 1)
    end
  }

  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'quangnguyen30192/cmp-nvim-ultisnips',
      { 'tzachar/cmp-tabnine', run='./install.sh' },
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-nvim-lua',
      'onsails/lspkind-nvim',
      { 'David-Kunz/cmp-npm', requires = 'nvim-lua/plenary.nvim' }
    },
    config = function()
      local lspkind = require'lspkind'

      lspkind.init({
        with_text = false,
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
        },
      })
      local cmp = require'cmp'

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
          end,
        },
        mapping = {
          ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 's' }),
          ['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 's' }),
          ['<C-d>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.close(),
          ['<CR>'] = cmp.mapping.confirm({ select = false }),
        },
        sources = {
          { name = 'nvim_lsp' },
          { name = 'cmp_tabnine'},
          { name = 'nvim_lua' },
          { name = 'buffer' },
          { name = 'path'},
          { name = 'npm', keyword_length = 4 },
          { name = 'ultisnips' }
        },
        documentation = {
          border = {
            "┌", "─", "┐", "│", "┘", "─", "└", "│"
          }
        }
      })
    end
  }

  use "glepnir/lspsaga.nvim"
  use "kosayoda/nvim-lightbulb"

  use {
    'RishabhRD/nvim-lsputils',
    requires = 'RishabhRD/popfix'
  }

  use "neovim/nvim-lspconfig"

  use {
    'jose-elias-alvarez/nvim-lsp-ts-utils',
    requires = {
      "jose-elias-alvarez/null-ls.nvim",
      config = function() require("null-ls").setup() end
    }
  }

  use {
    "SirVer/ultisnips",
    config = function()
      vim.api.nvim_set_var("UltiSnipsExpandTrigger", "<NUL>")
    end
  }

  use {
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    cmd = { "Trouble", "LspTrouble" },
    config = function()
      require("trouble").setup()
    end
  }

  use { 
    "weirongxu/plantuml-previewer.vim",
    ft = "plantuml",
    requires = {
      "tyru/open-browser.vim",
      "aklt/plantuml-syntax"
    }
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      local ts = require 'nvim-treesitter.configs'
      ts.setup {
        ensure_installed = 'maintained',
        highlight = {
          enable = true
        },
      }
    end
  }

  use {
    'benwainwright/fzf-project',
    -- '~/repos/fzf-project'
  }

  -- use {
  --   'marko-cerovac/material.nvim',
  --   config = function()
  --     vim.g.material_style = "deep ocean"
  --     vim.cmd('colorscheme material')
  --   end
  -- }

  use { 
    'projekt0n/github-nvim-theme',
    after = "lualine.nvim",
    config = function()
      require("github-theme").setup {
        theme_style = 'dark_default'
      }
    end
  }

  use {
    'TimUntersberger/neogit',
    cmd = "Neogit",
    config = function() require("neogit").setup() end
  }

  use {
    "lewis6991/gitsigns.nvim",
    config = function() require("gitsigns").setup() end
  }

  use { 
    "lbrayner/vim-rzip",
    config = function()
      vim.cmd([[
      " See https://yarnpkg.com/getting-started/editor-sdks#vim
      " Decode URI encoded characters
      function! DecodeURI(uri)
          return substitute(a:uri, '%\([a-fA-F0-9][a-fA-F0-9]\)', '\=nr2char("0x" . submatch(1))', "g")
      endfunction

      " Attempt to clear non-focused buffers with matching name
      function! ClearDuplicateBuffers(uri)
          " if our filename has URI encoded characters
          if DecodeURI(a:uri) !=# a:uri
              " wipeout buffer with URI decoded name - can print error if buffer in focus
              sil! exe "bwipeout " . fnameescape(DecodeURI(a:uri))
              " change the name of the current buffer to the URI decoded name
              exe "keepalt file " . fnameescape(DecodeURI(a:uri))
              " ensure we don't have any open buffer matching non-URI decoded name
              sil! exe "bwipeout " . fnameescape(a:uri)
          endif
      endfunction

      function! RzipOverride()
          " Disable vim-rzip's autocommands
          autocmd! zip BufReadCmd   zipfile:*,zipfile:*/*
          exe "au! zip BufReadCmd ".g:zipPlugin_ext

          " order is important here, setup name of new buffer correctly then fallback to vim-rzip's handling
          autocmd zip BufReadCmd   zipfile:*  call ClearDuplicateBuffers(expand("<amatch>"))
          autocmd zip BufReadCmd   zipfile:*  call rzip#Read(DecodeURI(expand("<amatch>")), 1)

          if has("unix")
              autocmd zip BufReadCmd   zipfile:*/*  call ClearDuplicateBuffers(expand("<amatch>"))
              autocmd zip BufReadCmd   zipfile:*/*  call rzip#Read(DecodeURI(expand("<amatch>")), 1)
          endif

          exe "au zip BufReadCmd ".g:zipPlugin_ext."  call rzip#Browse(DecodeURI(expand('<amatch>')))"
      endfunction

      autocmd VimEnter * call RzipOverride()
      ]])
    end
  }

  use {
    "hoob3rt/lualine.nvim",
    config = function()
      require("lualine").setup {
        options = {
          theme = "github",
          section_separators = {"", ""},
          component_separators = {"", ""},
        }
      }
    end
  }

end, config = { max_jobs = 50 }})
