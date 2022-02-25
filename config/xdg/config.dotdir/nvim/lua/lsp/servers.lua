local lsp_installer_servers = require('nvim-lsp-installer.servers')
local notify = require('notify')

local lsp_keymaps = require('lsp.keymaps')
local lsp_autocommands = require('lsp.autocommands')
local lsp_signs = require('lsp.signs')
local lsp_handlers = require('lsp.handlers')
local Delay = require('delay')

local M = {}

local execute_delays
execute_delays = function(opts)
  local newOpts = {}

  for k, v in pairs(opts) do
    if Delay.isDelay(v) then
      newOpts[k] = v.execute()
    elseif type(v) == 'table' then
      newOpts[k] = execute_delays(v)
    else
      newOpts[k] = v
    end
  end

  return newOpts
end

local configure_server = function(server_name, opts)
  local server_available, requested_server =
      lsp_installer_servers.get_server(
          server_name
      )
  local suppliedOpts = opts or {}

  local passed_in_on_attach = suppliedOpts.on_attach

  local wrapped_on_attach = function(client, bufnr)
    if type(passed_in_on_attach) == 'function' then
      passed_in_on_attach(client, bufnr)
    end
    lsp_keymaps.init()
    lsp_autocommands.init(client)
    lsp_signs.init()
    lsp_handlers.init()
  end

  if server_name == "sumneko_lua" then
    local luadev = require("lua-dev").setup({})
    for k, v in pairs(luadev) do suppliedOpts[k] = v end
  end

  suppliedOpts.on_attach = wrapped_on_attach

  if server_available then
    requested_server:on_ready(
        function()
          -- local finalOpts = execute_delays(suppliedOpts)
          requested_server:setup(suppliedOpts)
        end
    )
  end
  if not requested_server:is_installed() then requested_server:install() end
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
