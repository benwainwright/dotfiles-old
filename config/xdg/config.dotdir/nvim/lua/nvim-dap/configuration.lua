local dap = require("dap")

local node_dap_path = vim.fn.stdpath('data') .. '/dap/vscode-node-debug2'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  print('vscode-node-debug2 not found. Cloning...')
  vim.fn.system(
      {
        'git',
        'clone',
        'https://github.com/microsoft/vscode-node-debug2.git',
        node_dap_path
      }
  )

  print('installing dependencies...')
  os.execute('cd ' .. node_dap_path .. ' && npm install')
  os.execute('cd ' .. node_dap_path .. ' && gulp build')
  print('vscode-node-debug2 has been installed')
end

dap.adapters.node2 = function(callback, config)
  if config.preLaunchTask then vim.fn.system(config.preLaunchTask) end
  local adapter = {
    type = 'executable',
    command = 'node',
    args = {
      node_dap_path .. '/out/src/nodeDebug.js'
    }
  }
  callback(adapter)
end

dap.configurations.typescript = {
  {
    type = 'node2',
    cwd = vim.fn.getcwd(),
    sourceMaps = true,
    protocol = 'inspector',
    console = 'integratedTerminal'
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
    console = 'integratedTerminal'
  }
}

require('dap.ext.vscode').load_launchjs()
