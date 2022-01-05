local vim = require("nvim-api")

local M = {}

function M.init()
  vim.map {
    n = {
      {
        key = "<F9>",
        lua = "require'dap'.toggle_breakpoint()"
      },
      {
        key = "<F5>",
        lua = "require'dap'.continue()"
      },
      {
        key = "<F6>",
        lua = "require'dap'.step_over()"
      },
      {
        key = "<F7>",
        lua = "require'dap'.step_into()"
      },
      {
        key = "<leader>da",
        lua = "require'debug-helper'.attach()"
      }
    }
  }
end

return M
