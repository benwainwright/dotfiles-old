return require('packer').startup(function(use)

  use {

    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup()
    end
  }

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

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
end)
