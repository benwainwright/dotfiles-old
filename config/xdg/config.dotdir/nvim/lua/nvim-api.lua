local M = {}

M.options = {get = vim.api.nvim_get_option, set = vim.api.nvim_set_option}

function M.exec(command) vim.api.nvim_exec(command, false) end

function M.options.append(name, value)
    local currentValue = vim.api.nvim_get_option(name)
    vim.api.nvim_set_option(name, currentValue .. value)
end

function M.options.prepend(name, value)
    local currentValue = vim.api.nvim_get_option(name)
    vim.api.nvim_set_option(name, value .. currentValue)
end

function M.map(mode, keys, command, opts)
    local options = {noremap = true}
    if opts then options = vim.tbl_extend('force', options, opts) end
    vim.keymap.set(mode, keys, command, options)
end

function M.maps(maps)
    for mode, modeMaps in pairs(maps) do
        for _, map in ipairs(modeMaps) do
            if type(map.lua) == "function" then
                M.map(mode, map.key, map.lua, map.opts)
            elseif map.lua ~= nil then
                M.map(mode, map.key, "<cmd>lua " .. map.lua .. "<CR>", map.opts)
            else
                M.map(mode, map.key, "<cmd>" .. map.command .. "<CR>", map.opts)
            end
        end
    end
end

--- Define autocommands
-- @param definitions a table mapping autogroup names to
--        a list of individual autocommands
function M.define_autocommands(definitions)
    for group_name, definition in pairs(definitions) do
        local group_name_for_buffer = group_name .. '_' .. vim.fn.bufnr("%")
        vim.api.nvim_command('augroup ' .. group_name_for_buffer)
        vim.api.nvim_command('autocmd!')
        for _, def in ipairs(definition) do
            local command = table.concat(vim.tbl_flatten {'autocmd', def}, ' ')
            vim.api.nvim_command(command)
        end
        vim.api.nvim_command('augroup END')
    end
end

return M
