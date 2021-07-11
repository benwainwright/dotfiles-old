local M = {}

M.options = {
  get = vim.api.nvim_get_option,
  set = vim.api.nvim_set_option
}

function M.exec(command)
	vim.api.nvim_exec(command, false)
end

function M.options.append(name, value)
  local currentValue = vim.api.nvim_get_option(name)
  vim.api.nvim_set_option(name, currentValue .. value)
end

function M.options.prepend(name, value)
  local currentValue = vim.api.nvim_get_option(name)
  vim.api.nvim_set_option(name, value .. currentValue)
end

return M
