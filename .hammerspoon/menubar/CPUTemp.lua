--- CPU Temperature in menubar

local function init()
    CPUTempMenu = hs.menubar.new()
end

local function updateTemp()
    local temp = hs.execute("osx-cpu-temp", true)
    temp = temp:gsub("[\r\n]", "")
    CPUTempMenu:setTitle(temp)
end

local timer = nil
function ShowCPUTempMenu()
    init()

    if timer == nil then
        timer = hs.timer.doEvery(30, function() updateTemp() end)
        timer:setNextTrigger(0)
    else
        timer:start()
    end
end

function HideCPUTempMenu()
    CPUTempMenu:delete()
    timer:stop()
    timer = nil
end
