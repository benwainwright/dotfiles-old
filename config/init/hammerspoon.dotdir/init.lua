hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

local hotkey = hs.hotkey
local application = hs.application
local window = hs.window
local geometry = hs.geometry
local appfinder = hs.appfinder

local mash = {"cmd", "alt", "ctrl"}
local cmdCtrl = {"cmd", "alt"}
local spectacleshift = {"cmd", "alt", "shift"}
local cmdCtrl = {"cmd", "ctrl"}

hotkey.bind(cmdCtrl, 'E', function() application.launchOrFocus('Microsoft Teams') end)
hotkey.bind(cmdCtrl, 'T', function() application.launchOrFocus('Things3') end)
hotkey.bind(cmdCtrl, 'N', function() application.launchOrFocus('Evernote') end)
hotkey.bind(cmdCtrl, 'M', function() application.launchOrFocus('Spotify') end)
hotkey.bind(cmdCtrl, 'B', function() application.launchOrFocus('Google Chrome') end)
hotkey.bind(cmdCtrl, 'C', function() application.launchOrFocus('Calendar') end)
hotkey.bind(cmdCtrl, 'I', function() application.launchOrFocus('iTerm') end)
hotkey.bind(cmdCtrl, 'F', function() application.launchOrFocus('Finder') end)
hotkey.bind(mash, 'h', function() window.focusedWindow():moveOneScreenWest() end)
hotkey.bind(mash, 'l', function() window.focusedWindow():moveOneScreenEast() end)
hotkey.bind(mash, 'j', function() window.focusedWindow():moveOneScreenSouth() end)
hotkey.bind(mash, 'k', function() window.focusedWindow():moveOneScreenNorth() end)


function moveTo(win, x, y, h, w)
  local rect = geometry.rect(x, y, h, w)
  win:moveToUnit(rect)
end

function moveNamedAppTo(name, x, y, h, w)
    local app = appfinder.appFromName(name)
    moveTo(app:mainWindow(), x, y, h, w)
    app:mainWindow():focus()
end

hotkey.bind(cmdCtrl, "H", function()
    moveTo(window.focusedWindow(), 0, 0, 0.5, 1)
end)

hotkey.bind(cmdCtrl, "J", function()
    moveTo(window.focusedWindow(), 0, 0.5, 1, 0.5)
end)

hotkey.bind(cmdCtrl, "K", function()
    moveTo(window.focusedWindow(), 0, 0, 1, 0.5)
end)

hotkey.bind(cmdCtrl, "L", function()
    moveTo(window.focusedWindow(), 0.5, 0, 0.5, 1)
end)

hotkey.bind(spectacleshift, "H", function()
    moveTo(window.focusedWindow(), 0, 0, 0.5, 1)
    moveNamedAppTo('iTerm2', 0.5, 0, 0.5, 1)
end)

hotkey.bind(spectacleshift, "J", function()
    moveTo(window.focusedWindow(), 0, 0.5, 1, 0.5)
    moveNamedAppTo('iTerm2', 0, 0, 1, 0.5)
end)

hotkey.bind(spectacleshift, "K", function()
    moveTo(window.focusedWindow(), 0, 0, 1, 0.5)
    moveNamedAppTo('iTerm2', 0, 0.5, 1, 0.5)
end)

hotkey.bind(spectacleshift, "L", function()
    moveTo(window.focusedWindow(), 0.5, 0, 0.5, 1)
    moveNamedAppTo('iTerm2', 0, 0, 0.5, 1)
end)

-- Full screen.
hotkey.bind(cmdCtrl, "F", function()
    local win = window.focusedWindow():maximize()
end)
