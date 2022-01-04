local lsp_installer_servers = require('nvim-lsp-installer.servers')
local lsp_on_attach = require("lsp.lsp_on_attach")

local M = {}

function M.server(server_name, opts)
  local server_available, requested_server = lsp_installer_servers.get_server(server_name)
  local opts = opts or {}
  local passed_in_on_attach = opts.on_attach

  local wrapped_on_attach = function(client, bufnr)
    if type(passed_in_on_attach) == 'function' then
      passed_in_on_attach(client, bufnr)
    end
    lsp_on_attach.global_lsp_config(client, bufnr)
  end

  opts.on_attach = wrapped_on_attach

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
