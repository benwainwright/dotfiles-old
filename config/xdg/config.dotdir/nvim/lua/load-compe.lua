require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 200;
  source_timeout = 200;
  resolve_timeout = 800;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = {
    border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  };

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
    ultisnips = true;
    tabnine = true;
    luasnip = true;
  };
}

vim.api.nvim_set_var("completion_enable_snippet", "UltiSnips")

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

local is_prior_char_whitespace = function()
  local col = vim.fn.col('.') - 1
  if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
    return true
  else
    return false
  end
end

-- Use (shift-)tab to:
--- move to prev/next item in completion menu
--- jump to the prev/next snippet placeholder
--- insert a simple tab
--- start the completion menu
_G.tab_completion = function()
  if vim.fn.pumvisible() == 1 then
    return vim.api.nvim_replace_termcodes("<C-n>", true, true, true)

  elseif vim.fn["UltiSnips#CanExpandSnippet"]() == 1 or vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
    return vim.api.nvim_replace_termcodes("<C-R>=UltiSnips#ExpandSnippetOrJump()<CR>", true, true, true)

  elseif is_prior_char_whitespace() then
    return vim.api.nvim_replace_termcodes("<Tab>", true, true, true)

  else
    return vim.fn['compe#complete']()
  end
end

_G.shift_tab_completion = function()
  if vim.fn.pumvisible() == 1 then
    return vim.api.nvim_replace_termcodes("<C-p>", true, true, true)

  elseif vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
    return vim.api.nvim_replace_termcodes("<C-R>=UltiSnips#JumpBackwards()<CR>", true, true, true)

  else
    return vim.api.nvim_replace_termcodes("<S-Tab>", true, true, true)
  end
end


vim.api.nvim_set_keymap("i", "<C-Space>", "compe#complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<C-e>", "compe#close('<C-e>')", {expr = true})
vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_completion()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_completion()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.shift_tab_completion()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.shift_tab_completion()", {expr = true})
vim.api.nvim_set_keymap("i", "<CR>", "compe#confirm(luaeval('require \"nvim-autopairs\".autopairs_cr()'))", {expr = true})
