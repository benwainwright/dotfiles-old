local dap_keymaps = require('nvim-dap.keymaps')
local dap_configuration = require('nvim-dap.configuration')
local dap_signs = require('nvim-dap.signs')

local M = {}

function M.init()
  dap_keymaps.init()
  dap_configuration.init()
  dap_signs.init()
end

return M
