local ns = vim.api.nvim_create_namespace('initdotlua')
vim.api.nvim_set_hl_ns(ns)

vim.api.nvim_set_hl(
    ns, 'DiffAdd', {
      ctermbg = "NONE",
      ctermfg = "green"
    }
)

vim.api.nvim_set_hl(
    ns, 'DiffDelete', {
      ctermbg = "NONE",
      ctermfg = "red"
    }
)

vim.api.nvim_set_hl(
    ns, 'DiffChange', {
      ctermbg = "NONE",
      ctermfg = "yellow"
    }
)
