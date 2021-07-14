return require('packer').startup(function(use)

  use {
    "SirVer/ultisnips",
    config = function()
      vim.api.nvim_set_var("UltiSnipsExpandTrigger", "<NULL>")
    end
  }

  use {
    "prettier/vim-prettier"
  }

  use {
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup()
    end
  }

  use { 
    "weirongxu/plantuml-previewer.vim",
    requires = {
      "tyru/open-browser.vim",
      "aklt/plantuml-syntax"
    }
  }

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use { 'benwainwright/fzf-project' }

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

  use {
    'neovim/nvim-lspconfig',
  }
  use {
    'jose-elias-alvarez/nvim-lsp-ts-utils',
    {
      requires = {
        "jose-elias-alvarez/null-ls.nvim",
        config = function() require("null-ls").setup() end
      }
    }
  }
end)
