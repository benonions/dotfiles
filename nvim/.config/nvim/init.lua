-- Aesthetic
-- pcall catches errors if the plugin doesn't load
local ok, tokyonight = pcall(require, "dracula")
if not ok then return end
--vim.g.catppuccin_flavour = "frappe"
tokyonight.setup()
vim.cmd [[colorscheme dracula]]

require('mr_onions.options')
require('mr_onions.globals')
require('mr_onions.lualine')
require('mr_onions.keymap')
require('mr_onions.lsp')
require('mr_onions.telescope')
