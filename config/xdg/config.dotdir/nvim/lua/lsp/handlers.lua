local M = {}

M.init = function()
  local bufnr = vim.api.nvim_buf_get_number(0)

  vim.lsp.handlers['textDocument/codeAction'] =
  function(_, _, actions)
    require('lsputil.codeAction').code_action_handler(
      nil, actions, nil, nil, nil
    )
  end

  -- vim.lsp.handlers['textDocument/references'] = function(_, _, result)
  --     require('lsputil.locations').references_handler(nil, result, { bufnr = bufnr }, nil)
  -- end

  -- vim.lsp.handlers['textDocument/definition'] =
  -- function(_, method, result)
  --   require('lsputil.locations').definition_handler(
  --     nil, result, {
  --       bufnr = bufnr,
  --       method = method
  --     }, nil
  --   )
  -- end

  vim.lsp.handlers['textDocument/declaration'] =
  function(_, method, result)
    require('lsputil.locations').declaration_handler(
      nil, result, {
        bufnr = bufnr,
        method = method
      }, nil
    )
  end

  vim.lsp.handlers['textDocument/typeDefinition'] =
  function(_, method, result)
    require('lsputil.locations').typeDefinition_handler(
      nil, result, {
        bufnr = bufnr,
        method = method
      }, nil
    )
  end

  vim.lsp.handlers['textDocument/implementation'] =
  function(_, method, result)
    require('lsputil.locations').implementation_handler(
      nil, result, {
        bufnr = bufnr,
        method = method
      }, nil
    )
  end

  vim.lsp.handlers['textDocument/documentSymbol'] = function(
      _, _, result, _, bufn
  )
    require('lsputil.symbols').document_handler(
      nil, result, {
        bufnr = bufn
      }, nil
    )
  end

  vim.lsp.handlers['textDocument/symbol'] =
  function(_, _, result, _, bufn)
    require('lsputil.symbols').workspace_handler(
      nil, result, {
        bufnr = bufn
      }, nil
    )
  end

  vim.lsp.handlers["workspace/symbol"] =
  require 'fzf_lsp'.workspace_symbol_handler

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = true,
    underline = true,
    signs = true
  }
  )

  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
    vim.lsp.handlers.hover, {
    border = "single"
  }
  )
end

return M
