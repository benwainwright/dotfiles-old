local ns = vim.api.nvim_create_namespace('initdotlua')
vim.api.nvim__set_hl_ns(ns)

vim.api.nvim_set_hl(
    ns, 'DiffAdd', {
      ctermbg = "NONE",
      guibg = "NONE",
      ctermfg = "green",
      guifg = "green"
    }
)

vim.api.nvim_set_hl(
    ns, 'DiffDelete', {
      ctermbg = "NONE",
      guibg = "NONE",
      ctermfg = "red",
      guifg = "red"
    }
)

vim.api.nvim_set_hl(
    ns, 'DiffChange', {
      ctermbg = "NONE",
      guibg = "NONE",
      ctermfg = "yellow",
      guifg = "yellow"
    }
)


vim.api.nvim_set_hl(

    ns, "CocUnderline", {

      cterm = "undercurl",

      gui = "undercurl"

    }

)
