--- Horizontal Calendar Widget
---
--- Require:
---  * HCalendarTopLeft = {x, y}

local hCalendarBGColor = {red=0,blue=0,green=0,alpha=0.3}
local hCalendarTitleColor = {red=1,blue=1,green=1,alpha=0.3}
local offDayColor = {red=255/255,blue=120/255,green=120/255,alpha=1}
local todayColor = {red=1,blue=1,green=1,alpha=0.2}
local midlineColor = {red=1,blue=1,green=1,alpha=0.5}
local midlineTodayColor = {red=0,blue=1,green=186/255,alpha=0.8}
local hCalendarW = 31 * 24 + 20
local hCalendarH = 100
local isInit = false

local function init()
    isInit = true

    HCalendarWidgetCanvas = hs.canvas.new({
        x = HCalendarTopLeft[1],
        y = HCalendarTopLeft[2]-hCalendarH,
        w = hCalendarW,
        h = hCalendarH,
    }):show()

    HCalendarWidgetCanvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
    HCalendarWidgetCanvas:level(hs.canvas.windowLevels.desktopIcon)

    HCalendarWidgetCanvas[1] = {
        id = "hcal_bg",
        type = "rectangle",
        action = "fill",
        fillColor = hCalendarBGColor,
        roundedRectRadii = {xRadius = 10, yRadius = 10},
    }

    HCalendarWidgetCanvas[2] = {
        id = "hcal_title",
        type = "text",
        text = "",
        textSize = 18,
        textColor = hCalendarTitleColor,
        textAlignment = "left",
        frame = {
            x = tostring(10/hCalendarW),
            y = tostring(10/hCalendarH),
            w = tostring(1-20/hCalendarW),
            h = "30%"
        }
    }

    -- upside weekday string
    for i=3, 3+30 do
        HCalendarWidgetCanvas[i] = {
            type = "text",
            text = "",
            textFont = "Courier-Bold",
            textSize = 13,
            textAlignment = "center",
            frame = {
                x = tostring((10+24*(i-3))/hCalendarW),
                y = "45%",
                w = tostring(24/(hCalendarW-20)),
                h = "23%"
            }
        }
    end

    -- midline rectangle
    for i=34, 34+30 do
        HCalendarWidgetCanvas[i] = {
            type = "rectangle",
            action = "fill",
            fillColor = midlineColor,
            frame = {
                x = tostring((10+24*(i-34))/hCalendarW),
                y = "65%",
                w = tostring(24/(hCalendarW-20)),
                h = "4%"
            }
        }
    end

    -- downside day string
    for i=65, 65+30 do
        HCalendarWidgetCanvas[i] = {
            type = "text",
            text = "",
            textFont = "Courier-Bold",
            textSize = 13,
            textAlignment = "center",
            frame = {
                x = tostring((10+24*(i-65))/hCalendarW),
                y = "70%",
                w = tostring(24/(hCalendarW-20)),
                h = "23%"
            }
        }
    end

    -- today cover rectangle
    HCalendarWidgetCanvas[96] = {
        type = "rectangle",
        action = "fill",
        fillColor = todayColor,
        roundedRectRadii = {xRadius = 3, yRadius = 3},
        frame = {
            x = tostring(10/hCalendarW),
            y = "44%",
            w = tostring(24/(hCalendarW-20)),
            h = "46%"
        }
    }
end

local function updateCanvas()
    local titleString = os.date("%B %Y")
    HCalendarWidgetCanvas[2].text = titleString
    local currentYear = os.date("%Y")
    local currentMonth = os.date("%m")
    local currentDay = os.date("%d")
    local weekNames = {"Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"}
    local firstDayOfNextMonth = os.time{year=currentYear, month=currentMonth+1, day=1}
    local lastDayOfCurrentMonth = os.date("*t", firstDayOfNextMonth-24*60*60).day
    for i=1,31 do
        local weekdayOfDueriedDay = os.date("*t", os.time{year=currentYear, month=currentMonth, day=i}).wday
        local mappedWeekDayString = weekNames[weekdayOfDueriedDay]
        HCalendarWidgetCanvas[2+i].text = mappedWeekDayString
        HCalendarWidgetCanvas[64+i].text = i
        if mappedWeekDayString == "Sa" or mappedWeekDayString == "Su" then
            HCalendarWidgetCanvas[2+i].textColor = offDayColor
            HCalendarWidgetCanvas[33+i].fillColor = offDayColor
            HCalendarWidgetCanvas[64+i].textColor = offDayColor
        end
        if i == math.tointeger(currentDay) then
            HCalendarWidgetCanvas[33+i].fillColor = midlineTodayColor
            HCalendarWidgetCanvas[96].frame.x = tostring((10+24*(i-1))/hCalendarW)
        end
        -- hide extra day
        if i > lastDayOfCurrentMonth then
            HCalendarWidgetCanvas[2+i].textColor.alpha = 0
            HCalendarWidgetCanvas[33+i].fillColor.alpha = 0
            HCalendarWidgetCanvas[64+i].textColor.alpha = 0
        end
    end
    -- trim the canvas
    HCalendarWidgetCanvas:size({
        w = lastDayOfCurrentMonth*24+20,
        h = 100
    })
end

local timer = nil
function ShowHCalendar()
    if not isInit then
        init()
    else
        HCalendarWidgetCanvas:show()
    end

    if timer == nil then
        -- set timer every 45 min
        timer = hs.timer.doEvery(2700, function() updateCanvas() end)
        timer:setNextTrigger(0)
    else
        timer:start()
    end
end

function HideHCalendar()
    HCalendarWidgetCanvas:hide()
    timer:stop()
    timer = nil
end

