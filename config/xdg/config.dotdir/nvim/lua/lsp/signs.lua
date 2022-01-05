local M = {}

M.init = function()
  vim.fn.sign_define(
      "DiagnosticSignError", {
        text = "",
        texthl = "DiagnosticSignError"
      }
  )

  vim.fn.sign_define(
      "DiagnosticSignWarning", {
        text = "",
        texthl = "DiagnosticSignWarning"
      }
  )

  vim.fn.sign_define(
      "DiagnosticSignInformation", {
        text = "",
        texthl = "DiagnosticSignInformation"
      }
  )

  vim.fn.sign_define(
      "DiagnosticSignHint", {
        text = "",
        texthl = "DiagnosticSignHint"
      }
  )
end

return M
