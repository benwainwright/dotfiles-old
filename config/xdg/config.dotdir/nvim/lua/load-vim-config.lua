local nvim = require("nvim-api")
local options = nvim.options

local oldRunTimePath = options.get("runtimepath")
local runtimePath = oldRunTimePath .. ",~/.vim,~/.vim/after"

options.set("runtimepath", runtimePath)
options.set("packpath", runtimePath)

vim.api.nvim_exec("source ~/.vimrc", false)
