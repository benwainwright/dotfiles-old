local api = require('nvim-api')

local M = {}

local highlight_autocommands = {
  'CursorHold <buffer> lua vim.lsp.buf.document_highlight()',
  'CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()',
  'CursorMoved <buffer> lua vim.lsp.buf.clear_references()'
}

local lightbulb_autocommands = {
  'CursorHold <buffer> lua require"nvim-lightbulb".update_lightbulb()',
  'CursorHoldI <buffer> lua require"nvim-lightbulb".update_lightbulb()'
}

-- local formatting_autocommand = {
--   'BufWritePre <buffer> lua vim.lsp.buf.format()'
-- }

local concat = function(t1, t2)
  for _, v in ipairs(t2) do table.insert(t1, v) end
end

M.init = function(client)
  local caps = client.server_capabilities
  local name = client.name

  local autocommands = {}

  if caps.document_highlight == true then
    concat(autocommands, highlight_autocommands)
  end

  if caps.code_action ~= false then
    concat(autocommands, lightbulb_autocommands)
  end

  -- if caps.document_formatting == true then
  --   concat(autocommands, formatting_autocommand)
  -- end

  api.define_autocommands(
    {
      ['lsp_autocommands' .. name] = autocommands
    }
  )
end

return M
