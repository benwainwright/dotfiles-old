local M = {}

function M.init()
  vim.fn.sign_define(
      'DapBreakpoint', {
        text = '🛑',
        texthl = '',
        linehl = '',
        numhl = ''
      }
  )
  vim.fn.sign_define(
      'DapStopped', {
        text = '▶',
        texthl = '',
        linehl = '',
        numhl = ''
      }
  )
end

return M
