local lsp_installer_servers = require('nvim-lsp-installer.servers')

local M = {}

function M.server(server_name, opts)
  local server_available, requested_server = lsp_installer_servers.get_server(server_name)
  local opts = opts or {}
  if server_available then
    requested_server:on_ready(function ()
        requested_server:setup(opts)

    end)
  end
  if not requested_server:is_installed() then
      requested_server:install()
  end
end

return M
