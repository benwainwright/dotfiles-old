hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

local mash = {"cmd", "alt", "ctrl"}
local spectacle = {"cmd", "alt"}
local spectacleshift = {"cmd", "alt", "shift"}

hs.hotkey.bind(spectacle, 'M', function() hs.application.launchOrFocus('Spotify') end)
hs.hotkey.bind(spectacle, 'S', function() hs.application.launchOrFocus('Slack') end)
hs.hotkey.bind(spectacle, 'T', function() hs.application.launchOrFocus('Microsoft Outlook') end)
hs.hotkey.bind(spectacle, 'I', function() hs.application.launchOrFocus('iTerm') end)
hs.hotkey.bind(spectacle, 'B', function() hs.application.launchOrFocus('Safari') end)
hs.hotkey.bind(spectacle, 'E', function() hs.application.launchOrFocus('Evernote') end)
hs.hotkey.bind(spectacle, 'O', function() hs.application.launchOrFocus('Microsoft Outlook') end)
hs.hotkey.bind(spectacle, 'D', function() hs.application.launchOrFocus('Dash') end)
hs.hotkey.bind(spectacle, 'F', function() hs.application.launchOrFocus('Finder') end)
