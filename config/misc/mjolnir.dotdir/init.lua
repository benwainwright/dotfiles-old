local path = os.getenv("HOME") .. '/.luarocks/share/lua/5.2/?.lua'
local path_init = os.getenv("HOME") .. '/.luarocks/share/lua/5.2/?/init.lua'
local cpath = os.getenv("HOME") .. '/.luarocks/lib/lua/5.2/?.so'

package.path = package.path .. ';' .. path .. ';' .. path_init
package.cpath = package.cpath .. ';' .. cpath
local hotkey = require "mjolnir.hotkey"
local fnutils = require "mjolnir.fnutils"

hotkey.bind(mash, "2", function() os.execute("open \"focus://focus?minutes=25\"") end)
hotkey.bind(mash, 'C', function() application.launchorfocus('Calendar') end)
hotkey.bind(mash, 'E', function() application.launchorfocus('Preview') end)
hotkey.bind(mash, 'F', function() application.launchorfocus('Finder') end)
hotkey.bind(mash, 'H', function() application.launchorfocus('Google Chrome') end)
hotkey.bind(mash, 'I', function() application.launchorfocus('iTerm') end)
hotkey.bind(mash, 'J', function() application.launchorfocus('IntelliJ IDEA CE') end)
hotkey.bind(mash, 'K', function() application.launchorfocus('Kaleidoscope') end)
hotkey.bind(mash, 'L', function() application.launchorfocus('Charles') end)
hotkey.bind(mash, 'M', function() application.launchorfocus('MindNode') end)
hotkey.bind(mash, 'N', function() application.launchorfocus('iTunes') end)
hotkey.bind(mash, 'O', function() application.launchorfocus('Firefox') end)
hotkey.bind(mash, 'P', function() application.launchorfocus('Spotify') end)
hotkey.bind(mash, 'R', function() application.launchorfocus('OmniFocus') end)
hotkey.bind(mash, 'S', function() application.launchorfocus('Slack') end)
hotkey.bind(mash, 'T', function() application.launchorfocus('Contacts') end)
hotkey.bind(mash, 'U', function() application.launchorfocus('Mail') end)
hotkey.bind(mash, 'W', function() application.launchorfocus('1Password 7') end)
hotkey.bind(mash, 'X', function() application.launchorfocus('GitX') end)

