if vim.g.vscode then

else
    require("disable-builtins")
    require("bootstrap-packer")
    require("plugins")
    -- require("lsp.init")
    require("core-settings")
    require("keys")
    require("load-vim-config")
    require("custom-highlights")
end
