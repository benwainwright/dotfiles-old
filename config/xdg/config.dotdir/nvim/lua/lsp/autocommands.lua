local api = require('nvim-api')

local M = {}

M.init = function()
  api.define_autocommands {
    lsp_attach = {
      'CursorHold <buffer> lua vim.lsp.buf.document_highlight()',
      'CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()',
      'CursorMoved <buffer> lua vim.lsp.buf.clear_references()',
      'CursorHold <buffer> lua require"nvim-lightbulb".update_lightbulb()',
      'CursorHoldI <buffer> lua require"nvim-lightbulb".update_lightbulb()',
      'BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()'
    }
  }
end

return M
