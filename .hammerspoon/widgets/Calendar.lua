--- Calendar Widget

local BGColor = {red=0,blue=0,green=0,alpha=0.4}
local separatorColor  = {red=0,blue=1,green=186/255,alpha=0.8}
local whiteColor      = {red = 1, green = 1, blue = 1, alpha = 0.8 }
local canvasWidth  = 14 * 16 + 20
local canvasHeight = 100

-- External Display
local displaySize = {
    x=-1920.0, y=-180.0,
    w=1920.0, h=1080.0
}

local topLeft = {
    displaySize.x + 40,
    displaySize.y + displaySize.h - 40
}

local canvas = hs.canvas.new(
    {
        x = topLeft[1],
        y = topLeft[2]-canvasHeight,
        w = canvasWidth,
        h = canvasHeight
    }
)

canvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
canvas:level(hs.canvas.windowLevels.desktopIcon)

canvas[1] = {
    id = "background",
    type = "rectangle",
    action = "fill",
    fillColor = BGColor,
    roundedRectRadii = { xRadius = 10, yRadius = 10 }
}
canvas[2] = {
    id = "monthAndYear",
    type = "text",
    text = "",
    textFont = "Courier-Bold",
    textSize = 26,
    textAlignment = "center",
    frame = { x = 10, y = 3, h = 30, w = 220 }
}
canvas[3] = {
    id = "monthAndDayCover",
    type = "rectangle",
    action = "stroke",
    strokeColor = whiteColor,
    roundedRectRadii = { xRadius=3, yRadius=3 },
    frame = { x = 25-10, y = 40, h = 55, w = 30+10, }

}
canvas[4] ={
    id = "monthAndDaySeparator",
    type = "rectangle",
    action = "fill",
    fillColor = separatorColor,
    frame = { x = 25-10, y = 65, h = 5, w = 30+10, }

}
canvas[5] = {
    id = "monthAbbr",
    type = "text",
    text = "",
    textFont = "Courier-Bold",
    textSize = 18,
    frame = { x = 25, y = 40, h = 20, w = 30 }
}
canvas[6] = {
    id = "dayNumber",
    type = "text",
    text = "",
    textFont = "Courier-Bold",
    textSize = 18,
    frame = { x = 25, y = 70, h = 20, w = 30 }
}
canvas[7] = {
    id = "weekNumber",
    type = "text",
    text = "",
    textFont = "Courier-Bold",
    textSize = 18,
    frame = { x = 100, y = 40, h = 20, w = 200 }
}
canvas[8] = {
    id = "monthNumber",
    type = "text",
    text = "",
    textFont = "Courier-Bold",
    textSize = 18,
    frame = { x = 100, y = 70, h = 20, w = 200 }
}

local function updateWidget()
    local currentTimestamp = os.time()

    canvas[2].text = os.date("%B %Y", currentTimestamp)
    canvas[5].text = string.sub(tostring(os.date("%a", currentTimestamp)), 1, 2)
    canvas[6].text = os.date("%d", currentTimestamp)
    canvas[7].text = os.date("week:  %U", currentTimestamp)
    canvas[8].text = os.date("month: %m", currentTimestamp)
end

local initTimer = nil
local timer = nil
local function scheduleWidgetUpdate(status)
    local currentTimestamp = os.time()
    -- Calculating the time until the next widget update (~ 00:01 am)
    local delayH = tonumber(os.date("%H", currentTimestamp))*60*60
    local delayM  = tonumber(os.date("%M", currentTimestamp))*60
    local delayS  = tonumber(os.date("%S", currentTimestamp))
    local delay = 24*60*60 - delayH - delayM - delayS + 60

    -- Start timer
    if status == "start" then
        initTimer = hs.timer.doAfter(delay, function()
            updateWidget()
            initTimer:stop()
            timer = hs.timer.doEvery(24*60*60, updateWidget):start()
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
