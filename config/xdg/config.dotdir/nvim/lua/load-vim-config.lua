local oldRunTimePath = vim.api.nvim_get_option("runtimepath")
local runtimePath = oldRunTimePath .. ",~/.vim,~/.vim/after"

vim.api.nvim_set_option("runtimepath", runtimePath)
vim.api.nvim_set_option("packpath", runtimePath)

vim.api.nvim_exec("source ~/.vimrc", false)
