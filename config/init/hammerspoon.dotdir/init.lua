hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

local mash = {"cmd", "alt", "ctrl"}
local spectacle = {"cmd", "alt"}
local spectacleshift = {"cmd", "alt", "shift"}

hs.hotkey.bind(mash, 'M', function() hs.application.launchOrFocus('Spotify') end)
hs.hotkey.bind(mash, 'S', function() hs.application.launchOrFocus('Slack') end)
hs.hotkey.bind(mash, 'T', function() hs.application.launchOrFocus('Microsoft Outlook') end)
hs.hotkey.bind(mash, 'I', function() hs.application.launchOrFocus('iTerm') end)
hs.hotkey.bind(mash, 'B', function() hs.application.launchOrFocus('Safari') end)
hs.hotkey.bind(mash, 'E', function() hs.application.launchOrFocus('Evernote') end)
hs.hotkey.bind(mash, 'O', function() hs.application.launchOrFocus('Microsoft Outlook') end)
hs.hotkey.bind(mash, 'D', function() hs.application.launchOrFocus('Dash') end)
hs.hotkey.bind(mash, 'F', function() hs.application.launchOrFocus('Finder') end)
