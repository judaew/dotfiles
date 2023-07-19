--- Clock Widget

local BGColor = {red=0,blue=0,green=0,alpha=0.3}
local clockWidth = 5 * 53 + 20
local clockHeight =  100

-- externalDisplay
local displaySize = {x=-1920.0, y=-180.0, w=1920.0, h=1080.0}
local topLeft = {
    displaySize.x + displaySize.w - 40,
    displaySize.y + displaySize.h - 40
}

local canvas = hs.canvas.new({
    x = topLeft[1]-clockWidth,
    y = topLeft[2]-clockHeight,
    w = clockWidth,
    h = clockHeight,
})

canvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
canvas:level(hs.canvas.windowLevels.desktopIcon)

canvas[1] = {
    id = "clockBG",
    type = "rectangle",
    action = "fill",
    fillColor = BGColor,
    roundedRectRadii = {xRadius = 10, yRadius = 10},
}

canvas[2] = {
    id = "clockNumbers",
    type = "text",
    text = "",
    textSize = 80,
    textAlignment = "center",
    textFont = "Courier-Bold",
}

local function updateWidget()
    local currentTimestamp = os.time()
    local timeFormat = "%H:%M"

    canvas[2].text = os.date(timeFormat, currentTimestamp)
end

local initTimer = nil
local timer = nil
local function scheduleWidgetUpdate(status)
    local delay = 60 - tonumber(os.date("%S")) + 1

    -- Start timer
    if status == "start" then
        initTimer = hs.timer.doAfter(delay, function()
            updateWidget()
            initTimer:stop()
            timer = hs.timer.doEvery(60, function() updateWidget() end):start()
        end)
    else
        if initTimer or timer then
            initTimer:stop()
            timer:stop()
        end
        initTimer = nil
        timer = nil
    end
end

local M = {}

function M.show()
    canvas:show()
    updateWidget()
    scheduleWidgetUpdate("start")
end

function M.hide()
    canvas:hide()
    scheduleWidgetUpdate("stop")
end

return M
