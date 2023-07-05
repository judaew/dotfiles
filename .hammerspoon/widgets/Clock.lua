--- Clock Widget
---
--- Require:
---  * ClockTopLeft = {x, y}

local clockBGColor = {red=0,blue=0,green=0,alpha=0.3}
local clockW = 5 * 53 + 20
local clockH =  100
local isInit = false

local function init()
    isInit = true

    ClockWidgetCanvas = hs.canvas.new({
        x = ClockTopLeft[1]-clockW,
        y = ClockTopLeft[2]-clockH,
        w = clockW,
        h = clockH,
    }):show()

    ClockWidgetCanvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
    ClockWidgetCanvas:level(hs.canvas.windowLevels.desktopIcon)

    ClockWidgetCanvas[1] = {
        id = "clockBG",
        type = "rectangle",
        action = "fill",
        fillColor = clockBGColor,
        roundedRectRadii = {xRadius = 10, yRadius = 10},
    }

    ClockWidgetCanvas[2] = {
        id = "clockNumbers",
        type = "text",
        text = "",
        textSize = 80,
        textAlignment = "center",
        textFont = "Courier-Bold",
    }
end

local function updateCanvas()
    local timeFormat = "%H:%M"
    ClockWidgetCanvas[2].text = os.date(timeFormat)
end

local timer = nil
function ShowClock()
    if not isInit then
        init()
    else
        ClockWidgetCanvas:show()
    end

    if timer == nil then
        timer = hs.timer.doEvery(30, function() updateCanvas() end)
        timer:setNextTrigger(0)
    else
        timer:start()
    end
end

function  HideClock()
    ClockWidgetCanvas:hide()
    timer:stop()
    timer = nil
end
