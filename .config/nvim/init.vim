" ######################
" ### NVIM INIT FILE ###
" ######################
"
" Use vim config instead of lua, because nvimpager doesn't support it yet.
" See https://github.com/lucc/nvimpager/issues/64

" Basic settings
lua require 'options'
lua require 'autocommands'
lua require 'keymaps'

" Plugins and their setup
lua require 'plugins'
lua require 'init/packer_compiled'
