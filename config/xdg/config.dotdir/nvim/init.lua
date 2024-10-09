if vim.g.vscode then

else
    require("disable-builtins")
    require("keys")
    require("core-settings")

    -- Lazy should be loaded after keys as it requires leader and local leader to be set
    require("lazy-bootstrap")
    require("load-vim-config")
    require("custom-highlights")
end
