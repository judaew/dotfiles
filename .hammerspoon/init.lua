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

local screenCount = #hs.screen.allScreens()

-- function internalDisplay()
--     return hs.screen.find("1440x900")
-- end
-- local internalDisplaySize
-- if screenCount > 0 then
--     internalDisplaySize = internalDisplay():fullFrame()
-- end

-- function externalDisplay()
--     return hs.screen.allScreens()[2]
-- end
-- local externalDisplaySize = nil
-- if screenCount > 1 then
--     externalDisplaySize = externalDisplay():fullFrame()
-- end

local internalDisplaySize = {x=0.0, y=0.0, w=1440.0, h=900.0}
local externalDisplaySize = {x=-1920.0, y=-180.0, w=1920.0, h=1080.0}

local function loadWidgets()

    require("widgets/Clock")
    require("widgets/HCalendar")

    if screenCount == 2 then
        HCalendarTopLeft = {
                externalDisplaySize.x + 40,
                externalDisplaySize.y + externalDisplaySize.h - 40
        }
        ShowHCalendar()

        ClockTopLeft = {
            externalDisplaySize.x + externalDisplaySize.w - 40,
            externalDisplaySize.y + externalDisplaySize.h - 40
        }
        ShowClock()
    end
end

loadWidgets()
ScreenWatcher = hs.screen.watcher.new(hs.reload):start()
