local vim = require("nvim-api")

vim.exec("let mapleader = \"\\<Space>\"")
vim.exec("let maplocalleader = \"\\\\\"")

vim.maps {
    n = {{{key = "<space>jd", vscodeNotify = "editor.action.revealDefinition"}}}
}
