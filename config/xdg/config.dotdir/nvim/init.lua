if not vim.g.vscode then
  require("disable-builtins")
  require("bootstrap-packer")
  require("plugins")
  -- require("lsp.init")
  require("core-settings")
  require("keys")
  require("load-vim-config")
  require("custom-highlights")
end
