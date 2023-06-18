-- New window of the Terminal
hs.hotkey.bind({"option"}, "return", function()
    local app = hs.application("kitty")
    if app and app:bundleID() == "net.kovidgoyal.kitty" then
        app:selectMenuItem({"Shell", "New OS Window"})
    else
        hs.application.launchOrFocus("kitty")
    end
end)

function DropDownTerminal()
    local task = hs.task.new(
        "/Applications/MacPorts/kitty.app/Contents/MacOS/kitty",
        nil, function() return false end,
        {
            "-T", "Scratch",
            "-c", "/Users/judaew/.config/kitty/kitty.conf",
            "-c", "/Users/judaew/.config/kitty/drop-down.conf",
            "--hold", "sh", "-c", "cd ~; /opt/local/bin/tmux new -As Scratch"
        }
    )
    task:start()
end

-- Drop Down Terminal
hs.hotkey.bind({"option"}, "d", function()
    local app = hs.application('Scratch')
    if app then
        local terminal = hs.application.get(app:pid())
        if terminal:isFrontmost() then
            terminal:hide()
        else
            terminal:activate()
        end
    else
        DropDownTerminal()
    end
end)
