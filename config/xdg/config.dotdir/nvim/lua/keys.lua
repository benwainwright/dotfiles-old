local vim = require("nvim-api")

vim.exec("let mapleader = \"\\<Space>\"")
vim.exec("let maplocalleader = \"\\\\\"")

vim.maps {
  n = {
    {
      key = "<leader>o",
      command = "Other"
    },
    {
      key = "<C-e>",
      command = "NvimTreeFindFileToggle"
    },
    {
      key = "<leader>G",
      command = "Ag"
    },
    {
      key = "<C-b>",
      command = "Buffers"
    },
    {
      key = "<leader>cp",
      command = "vs ~/.config/nvim/lua/plugins.lua"
    },
    {
      key = "<leader>cp",
      command = "vs ~/.config/nvim/lua/keys.lua"
    },
    {
      key = "<leader>cp",
      command = "vs ~/.config/nvim/lua/lsp.lua"
    }
  }
}
