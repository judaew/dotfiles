vim.cmd([[
if has('mac')
    set rtp+=/opt/local/share/fzf/vim
else
    set rtp+=/usr/bin/fzf
endif
]])
