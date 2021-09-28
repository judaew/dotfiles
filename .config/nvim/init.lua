-- ######################
-- ### NVIM INIT FILE ###
-- ######################

-- Basic settings
vim.api.nvim_command 'runtime viml/init/config.vim'
vim.api.nvim_command 'runtime viml/init/autogroups.vim'
require 'init/mappings'

-- Plugins and their setup
require 'plugin/packer'
require 'init/packer_compiled'

-- Functions
vim.api.nvim_command 'runtime viml/func/last_jump.vim'
