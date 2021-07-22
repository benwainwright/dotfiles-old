return require('packer').startup(function(use)

  use {
    'mg979/vim-visual-multi',
    branch = "master"
  }

  use {
    "ray-x/lsp_signature.nvim",
  }

  -- use {
  --   'akinsho/nvim-toggleterm.lua',
  --   config = function()
      
  --     require("toggleterm").setup {
  --       -- size can be a number or function which is passed the current terminal
  --       size = function(term)
  --         if term.direction == "horizontal" then
  --           return 15
  --         elseif term.direction == "vertical" then
  --           return vim.o.columns * 0.4
  --         end
  --       end,

  --       open_mapping = [[<c-\>]],
  --       hide_numbers = true, -- hide the number column in toggleterm buffers
  --       shade_filetypes = {},
  --       shade_terminals = true,
  --       shading_factor = '1', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  --       start_in_insert = true,
  --       insert_mappings = true, -- whether or not the open mapping applies in insert mode
  --       persist_size = true,
  --       direction = 'horizontal',
  --       close_on_exit = true, -- close the terminal window when the process exits
  --       shell = vim.o.shell, -- change the default shell
  --       -- This field is only relevant if direction is set to 'float'
  --       float_opts = {
  --         -- The border key is *almost* the same as 'nvim_win_open'
  --         -- see :h nvim_win_open for details on borders however
  --         -- the 'curved' border is a custom border type
  --         -- not natively supported but implemented in this plugin.
  --         border = 'single',
  --         winblend = 3,
  --         highlights = {
  --           border = "Normal",
  --           background = "Normal",
  --         }
  --       }
  --     }
  --   end
  -- }

  use {
    'glepnir/dashboard-nvim',
    requires="liuchengxu/vim-clap"
  }

  -- Common functions for LUA plugins
  use 'nvim-lua/plenary.nvim'

  -- Add marks to sign column
  use 'kshenoy/vim-signature'

  -- Add extra text objects
  use 'wellle/targets.vim'

  -- Rainbow parens
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
          open_on_start = true,
          elements = {
            -- You can change the order of elements in the sidebar
            "scopes",
            "breakpoints",
            "stacks",
            "watches"
          },
          width = 40,
          position = "left" -- Can be "left" or "right"
        },
        tray = {
          open_on_start = true,
          elements = {
            "repl"
          },
          height = 10,
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


  use 'honza/vim-snippets'
  use 'mlaursen/vim-react-snippets'

  -- Behave sensibly with swap files
  use 'gioele/vim-autoswap'
  use 'ryanoasis/vim-devicons'
  use 'chrisbra/unicode.vim'

  use { 
    'sindrets/diffview.nvim',
    cmd = "DiffviewOpen"
  }

  use {
     'windwp/nvim-autopairs',
     config = function() require('nvim-autopairs').setup() end
  }

  use 'bkad/CamelCaseMotion'
  use 'PeterRincker/vim-argumentative'
  use 'michaeljsmith/vim-indent-object'

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
    cmd = "NvimTreeToggle",
    config = function()
      vim.api.nvim_set_var("nvim_tree_highlight_opened_files", 1)
      vim.api.nvim_set_var("nvim_tree_git_hl", 1)
      vim.api.nvim_set_var("nvim_tree_lsp_diagnostics", 1)
      vim.api.nvim_set_var("nvim_tree_update_cwd", 1)
      vim.api.nvim_set_var("nvim_tree_follow", 1)
    end
  }

  use {
    'hrsh7th/nvim-compe',
    event = "InsertEnter",
    config = function() require("load-compe") end
  }

  use {
    'tzachar/compe-tabnine',
    run = "./install.sh",
    requires = "hrsh7th/nvim-compe",
    event = "InsertEnter"
  }

  use "glepnir/lspsaga.nvim"
  use 'kosayoda/nvim-lightbulb'
  use {
    'RishabhRD/nvim-lsputils',
    requires = 'RishabhRD/popfix'
  }

  use 'kabouzeid/nvim-lspinstall'
  use 'neovim/nvim-lspconfig'

  use {
    'jose-elias-alvarez/nvim-lsp-ts-utils',
    requires = {
      "jose-elias-alvarez/null-ls.nvim",
      config = function() require("null-ls").setup() end
    }
  }

  use 'kyazdani42/nvim-web-devicons'
  use 'RishabhRD/popfix'
  use 'onsails/lspkind-nvim'
  use 'ynkdir/vim-vimlparser'
  use "knubie/vim-kitty-navigator"
  use "junegunn/fzf.vim"
  use "junegunn/fzf"
  use 'gfanto/fzf-lsp.nvim'
  use 'tpope/vim-rhubarb'
  use 'tpope/vim-fugitive'
  use 'tpope/vim-surround'
  use 'tpope/vim-dispatch'
  use 'tpope/vim-eunuch'
  use 'tpope/vim-commentary'
  use "fladson/vim-kitty"

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
      ts.setup {ensure_installed = 'maintained', highlight = {enable = true}}
    end
  }

  use {
    'benwainwright/fzf-project',
    cmd = { "FzfSwitchProject", "FzfChooseProjectFile" }
  }

  use { 
    'projekt0n/github-nvim-theme',
    config = function()
      require("github-theme").setup {
        darkFloat = true,
        darkSidebar = true
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
    "hoob3rt/lualine.nvim",
    config = function()
      require("lualine").setup {
        options = {
          theme = 'github',
          section_separators = {"", ""},
          component_separators = {"", ""},
        }
      }
    end
  }

end)
