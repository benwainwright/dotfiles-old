local vim = require("nvim-api")

vim.exec("let mapleader = \"\\<Space>\"")
vim.exec("let maplocalleader = \"\\\\\"")

vim.map("n", "<C-e>", "<cmd>NvimTreeToggle<CR>")
vim.map("n", "<leader>G", "<cmd>Ag<CR>")
vim.map("n", "<C-b>", "<cmd>Buffers<CR>")
-- map("n", "<C-j>", "<cmd>m .+1<CR>==")
-- map("n", "<C-k>", "<cmd>m .-2<CR>==")
-- map("i", "<C-j>", "<cmd>m .+1<CR>==gi")
-- map("i", "<C-k>", "<cmd>m .-2<CR>==gi")
