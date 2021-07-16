return require('packer').startup(function(use)

  -- Fancy splash screen
  use 'mhinz/vim-startify'

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

  -- Velocity syntax plugin
  use {
    'lepture/vim-velocity',
    ft = "velocity"
  }

  -- use 'mfussenegger/nvim-dap'
  
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
    'jiangmiao/auto-pairs',
    event = "InsertEnter",
  }

  use 'bkad/CamelCaseMotion'
  use 'PeterRincker/vim-argumentative'
  use 'michaeljsmith/vim-indent-object'

  use {
    'vim-test/vim-test',
    cmd = { "TestNearest", "TestFile", "TestSuite", "TestLast", "TestVisit" }
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
  use 'RishabhRD/nvim-lsputils'
  use 'kabouzeid/nvim-lspinstall'
  use 'neovim/nvim-lspconfig'

  use {
    'jose-elias-alvarez/nvim-lsp-ts-utils',
    {
      requires = {
        "jose-elias-alvarez/null-ls.nvim",
        config = function() require("null-ls").setup() end
      }
    }
  }

  use 'kyazdani42/nvim-web-devicons'
  use 'RishabhRD/popfix'
  use 'onsails/lspkind-nvim'
  use 'ynkdir/vim-vimlparser'
  use { 
    "sbdchd/neoformat",
    cmd = "Neoformat"
  }

  use "knubie/vim-kitty-navigator"
  use "junegunn/fzf.vim"
  use "junegunn/fzf"
  use "mfussenegger/nvim-dap"
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
