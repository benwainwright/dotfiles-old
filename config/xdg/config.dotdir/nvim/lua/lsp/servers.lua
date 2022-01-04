local lsp_installer_servers = require('nvim-lsp-installer.servers')

local lsp_keymaps = require('lsp.keymaps')
local lsp_autocommands = require('lsp.autocommands')
local lsp_signs = require('lsp.signs')
local lsp_handlers = require('lsp.handlers')

local M = {}

local configure_server = function(server_name, opts)
  local server_available, requested_server = lsp_installer_servers.get_server(server_name)
  local opts = opts or {}
  local passed_in_on_attach = opts.on_attach

  local wrapped_on_attach = function(client, bufnr)
    if type(passed_in_on_attach) == 'function' then
      passed_in_on_attach(client, bufnr)
    end
    lsp_keymaps.init(client)
    lsp_autocommands.init(client)
    lsp_signs.init(client)
    lsp_handlers.init(client)
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

M.configure = function(servers)
  for _, server in ipairs(servers) do
    if type(server) == 'string' then
      configure_server(server)
    else
      configure_server(server.name, server.options)
    end
  end
end

return M
