local api = require("nvim-api")
local dap = require("dap")
local debugHelper = require("debug-helper")

vim.fn.sign_define('DapBreakpoint', {text='🛑', texthl='', linehl='', numhl=''})
vim.fn.sign_define('DapStopped', {text='▶', texthl='', linehl='', numhl=''})

api.map("n", "<F9>", '<cmd>lua require"dap".toggle_breakpoint()<CR>')
api.map("n", "<F5>", '<cmd>lua require"dap".continue()<CR>')
api.map("n", "<F6>", '<cmd>lua require"dap".step_over()<CR>')
api.map("n", "<F7>", '<cmd>lua require"dap".step_into()<CR>')
api.map("n", "<leader>da", '<cmd>lua require"debug-helper".attach()')

dap.adapters.node2 = function(callback, config) 
  if config.preLaunchTask then vim.fn.system(config.preLaunchTask) end
  local adapter = {
    type = 'executable',
    command = 'node',
    args = {os.getenv('HOME') .. '/repos/vscode-node-debug2/out/src/nodeDebug.js'}
  }
  callback(adapter)
end

dap.configurations.typescript = {
  {
    type = 'node2',
    cwd = vim.fn.getcwd(),
    sourceMaps = true,
    protocol = 'inspector',
    console = 'integratedTerminal',
  }
}

dap.configurations.javascript = {
  {
    type = 'node2',
    request = 'launch',
    program = '${workspaceFolder}/${file}',
    cwd = vim.fn.getcwd(),
    sourceMaps = true,
    protocol = 'inspector',
    console = 'integratedTerminal',
  },
}

require('dap.ext.vscode').load_launchjs()
