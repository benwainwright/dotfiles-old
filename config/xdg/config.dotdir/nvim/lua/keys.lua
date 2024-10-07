local vim = require("nvim-api")

vim.exec("let mapleader = \"\\<Space>\"")
vim.exec("let maplocalleader = \"\\\\\"")

vim.maps {
    n = {
        {key = "<C-j>", command = "NavigatorDown"},
        {key = "<C-k>", command = "NavigatorUp"},
        {key = "<C-h>", command = "NavigatorLeft"},
        {key = "<C-l>", command = "NavigatorRight"},
        {key = "<leader>tr", lua = "require('neotest').run.run()"},
        {key = "nd", lua = "require('notify').dismiss()"},
        {key = "nh", lua = "require('notify').history()"},
        {key = "<leader>o", command = "Other"},
        {
            key = "<C-e>",
            lua = "require('nvim-tree.api').tree.toggle({find_file=true})"
        }, {key = "<leader>G", command = "Ag"},
        {key = "<C-b>", command = "Buffers"},
        {key = "<leader>cp", command = "vs ~/.config/nvim/lua/plugins.lua"},
        {key = "<leader>cp", command = "vs ~/.config/nvim/lua/keys.lua"},
        {key = "<leader>cp", command = "vs ~/.config/nvim/lua/lsp.lua"}
    }
}
