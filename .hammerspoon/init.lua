-- New window of the Terminal
hs.hotkey.bind({"option"}, "return", function()
    local app = hs.application("kitty")
    if app and app:bundleID() == "net.kovidgoyal.kitty" then
        app:selectMenuItem({"Shell", "New OS Window"})
    else
        hs.application.launchOrFocus("kitty")
    end
end)

-- Reload config file
hs.hotkey.bind({"option", "shift"}, "R", function() hs.reload() end)

-- Move to the next screen
hs.hotkey.bind({"option"}, "s", function()
    local app = hs.window.focusedWindow()
    app:moveToScreen(app:screen():next(), false, true)
end)

--- # Widgets #
--- ###########

local clock = require("widgets/Clock")
local cal = require("widgets/Calendar")

local screens = hs.screen.allScreens()

if #screens > 1 then
    cal.show()
    clock.show()
else
    cal.hide()
    clock.hide()
end

hs.screen.watcher.new(function() hs.timer.doAfter(2, hs.reload) end):start()

--- # Menubar #
--- ###########

require("menubar/CPUTemp").show()
