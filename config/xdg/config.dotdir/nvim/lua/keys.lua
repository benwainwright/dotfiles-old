local vim = require("nvim-api")

local function toggle_nvim_tree()
    local api = require("nvim-tree.api")

    local treeIsVisible = api.tree.is_visible({any_tabpage = true})
    print("treeIsVisible" .. tostring(treeIsVisible))

    if treeIsVisible then
        print("visible")
        local treeWin = api.tree.winid()
        print("win" .. treeWin)
        vim.api.nvim_set_current_win(treeWin)
        print("set")
        vim.cmd('q')
        print("q")
    else
        print("open")
        api.tree.open({find_file = true})
    end
end

vim.exec("let mapleader = \"\\<Space>\"")
vim.exec("let maplocalleader = \"\\\\\"")

vim.maps {
    n = {
        {key = "<C-j>", command = "NavigatorDown"},
        {key = "<C-k>", command = "NavigatorUp"},
        {key = "<C-h>", command = "NavigatorLeft"},
        {key = "<C-l>", command = "NavigatorRight"},
        {key = "<leader>tr", lua = "require('neotest').run.run()"},
        {key = "<leader>tw", lua = "require('neotest').watch.toggle()"},
        {key = "<leader>to", lua = "require('neotest').output_panel.toggle()"},
        {key = "nd", lua = "require('notify').dismiss()"},
        {key = "nh", lua = "require('notify').history()"},
        {key = "<leader>o", command = "Other"},
        {key = "<leader>G", command = "Ag"},
        {key = "<C-b>", command = "Buffers"},
        {key = "<leader>cp", command = "vs ~/.config/nvim/lua/plugins.lua"},
        {key = "<leader>cp", command = "vs ~/.config/nvim/lua/keys.lua"},
        {key = "<leader>cp", command = "vs ~/.config/nvim/lua/lsp.lua"}
    }
}
