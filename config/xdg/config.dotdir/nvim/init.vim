set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc
lua << EOF
  -- require("lsp")
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
      colors = {
        DiffAdd = "green"
      }
    }
  }
EOF


highlight DiffAdd ctermbg=NONE guibg=NONE ctermfg=green guifg=green 
highlight DiffDelete ctermbg=NONE guibg=NONE ctermfg=red guifg=red
highlight DiffChange ctermbg=NONE guibg=NONE ctermfg=yellow guifg=yellow
highlight CocUnderline cterm=undercurl gui=undercurl
