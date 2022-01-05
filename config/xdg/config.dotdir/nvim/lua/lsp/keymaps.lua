local vim = require("nvim-api")

local M = {}

function M.init()
  vim.maps {
    n = {
      {
        key = "<space>jD",
        lua = "vim.lsp.buf.declaration()"
      },
      {
        key = "<space>jd",
        lua = "vim.lsp.buf.definition()"
      },
      {
        key = "K",
        lua = "vim.lsp.buf.hover()"
      },
      {
        key = "<space>ji",
        lua = "vim.lsp.buf.implementation()"
      },
      {
        key = "<C-k>",
        lua = "vim.lsp.buf.signature_help()"
      },
      {
        key = "<space>wa",
        lua = "vim.lsp.buf.add_workspace_folder()"
      },
      {
        key = "<space>wr",
        lua = "vim.lsp.buf.remove_workspace_folder()"
      },
      {
        key = "<space>wl",
        lua = "print(vim.inspect(vim.lsp.buf.list_workspace_folders()))"
      },
      {
        key = "<space>D",
        lua = "vim.lsp.buf.type_definition()"
      },
      {
        key = "<space>r",
        lua = "require('cosmic-ui').rename()"
      },
      {
        key = "<space>a",
        lua = "vim.lsp.buf.code_action()"
      },
      {
        key = "<space>jr",
        lua = "vim.lsp.buf.references()"
      },
      {
        key = "<space>jr",
        lua = "vim.lsp.buf.references()"
      },
      {
        key = "<space>e",
        lua = "vim.diagnostic.open_float(nil, { border = 'single' })"
      },
      {
        key = "[d",
        lua = "vim.diagnostic.goto_prev({ float = { border = 'single' } })"
      },
      {
        key = "]d",
        lua = "vim.diagnostic.goto_next({ float = { border = 'single' } })"
      },
      {
        key = "<space>q",
        lua = "vim.lsp.diagnostic.set_loclist()"
      },
      {
        key = "<space>f",
        lua = "require'lspsaga.provider'.lsp_finder()"
      },
      {
        key = "<space>S",
        lua = "require'telescope.builtin'.lsp_dynamic_workspace_symbols {}"
      }
    },
    x = {
      {
        key = "<space>a",
        lua = "vim.lsp.buf.code_action()"
      }
    }
  }
end

return M
