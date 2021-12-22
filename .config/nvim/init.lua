-- ######################
-- ### NVIM INIT FILE ###
-- ######################

-- Basic settings
vim.api.nvim_command 'runtime config.vim'
vim.api.nvim_command 'runtime autogroups.vim'
vim.api.nvim_command 'runtime mappings.vim'

-- Plugins and their setup
require 'plugin/packer'
require 'init/packer_compiled'

-- Functions
vim.api.nvim_command 'runtime viml/func/last_jump.vim'
