require("trouble").setup()
require("treesitter")
require("github-theme").setup {
  darkFloat = true,
  darkSidebar = true
}
require("neogit").setup()
require('gitsigns').setup()
require("lualine").setup {
  options = {
    theme = 'github',
    section_separators = {"", ""},
    component_separators = {"", ""},
  }
}
