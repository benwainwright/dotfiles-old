local wezterm = require 'wezterm';

local act = wezterm.action

-- The next bit enables NeoVim Navigator.nvim integration.
-- See: See https://github.com/numToStr/Navigator.nvim/wiki/WezTerm-Integration
local function isViProcess(pane)
    -- get_foreground_process_name On Linux, macOS and Windows, 
    -- the process can be queried to determine this path. Other operating systems 
    -- (notably, FreeBSD and other unix systems) are not currently supported
    return pane:get_foreground_process_name():find('n?vim') ~= nil or
               pane:get_title():find("n?vim") ~= nil
end

local function conditionalActivatePane(window, pane, pane_direction,
                                       vim_direction)
    if isViProcess(pane) then
        window:perform_action( -- This should match the keybinds you set in Neovim.
        act.SendKey({key = vim_direction, mods = 'CTRL'}), pane)
    else
        window:perform_action(act.ActivatePaneDirection(pane_direction), pane)
    end
end

wezterm.on('ActivatePaneDirection-right', function(window, pane)
    conditionalActivatePane(window, pane, 'Right', 'l')
end)
wezterm.on('ActivatePaneDirection-left', function(window, pane)
    conditionalActivatePane(window, pane, 'Left', 'h')
end)
wezterm.on('ActivatePaneDirection-up', function(window, pane)
    conditionalActivatePane(window, pane, 'Up', 'k')
end)
wezterm.on('ActivatePaneDirection-down', function(window, pane)
    conditionalActivatePane(window, pane, 'Down', 'j')
end)

return {
    font = wezterm.font("FiraCode Nerd Font Mono"),
    window_padding = {left = 0, right = 0, top = 0, bottom = 0},
    font_size = 15,
    hide_tab_bar_if_only_one_tab = true,
    initial_cols = 120,
    initial_rows = 100,
    leader = {key = 'a', mods = 'CTRL'},
    keys = {

        {
            key = 'h',
            mods = 'CTRL',
            action = act.EmitEvent('ActivatePaneDirection-left')
        }, {
            key = 'j',
            mods = 'CTRL',
            action = act.EmitEvent('ActivatePaneDirection-down')
        },
        {
            key = 'k',
            mods = 'CTRL',
            action = act.EmitEvent('ActivatePaneDirection-up')
        }, {
            key = 'l',
            mods = 'CTRL',
            action = act.EmitEvent('ActivatePaneDirection-right')
        }, {
            key = 'k',
            mods = 'LEADER',
            action = wezterm.action.SplitPane {direction = 'Up'}
        }, {
            key = 'l',
            mods = 'LEADER',
            action = wezterm.action.SplitPane {direction = 'Right'}
        }, {
            key = 'h',
            mods = 'LEADER',
            action = wezterm.action.SplitPane {direction = 'Left'}
        }, {
            key = 'j',
            mods = 'LEADER',
            action = wezterm.action.SplitPane {direction = 'Down'}
        },
        {key = 'f', mods = 'LEADER', action = wezterm.action.ToggleFullScreen},
        {
            key = 'z',
            mods = 'LEADER',
            action = wezterm.action.TogglePaneZoomState
        }
    }
}
