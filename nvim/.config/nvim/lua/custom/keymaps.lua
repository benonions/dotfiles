-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
-- document existing key chains
-- require('which-key').register {
--   -- ['<leader>l'] = { name = 'lsp', _ = 'which_key_ignore' },      -- group
--   --
--   -- ['<leader>g'] = { name = '[G]it', _ = 'which_key_ignore' },    -- group
--   -- ['<leader>s'] = { name = '[S]earch', _ = 'which_key_ignore' }, --
--   -- ['leader>d'] = { name = 'debug', _ = 'which_key_ignore' },
--   -- ['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
--   -- ['<leader>e'] = { name = 'Neotree', _ = 'which_key_ignore' },
--   -- ['<leader>r'] = {
--   --   name = "refactoring", -- group name
--   --   r = { "<cmd>lua require('telescope').extensions.refactoring.refactors()<CR>", "Select refactor" },
--   --   lf = { "<cmd>lua require('refactoring').debug.printf({ below = false })<CR>", "Print" },
--   --   lv = { "<cmd>lua require('refactoring').debug.print_var()<CR>", "Print Variable" },
--   --   lc = { "<cmd>lua require('refactoring').debug.cleanup({})<CR>", "Print Cleanup" },
--   -- },
-- }

require('which-key').add {
  { "<leader>r",   group = "refactoring" },
  { "<leader>rr",  "<cmd>lua require('telescope').extensions.refactoring.refactors()<CR>", desc = "Select refactor" },
  { "<leader>rlf", "<cmd>lua require('refactoring').debug.printf({ below = false })<CR>",  desc = "Print" },
  { "<leader>rlv", "<cmd>lua require('refactoring').debug.print_var()<CR>",                desc = "Print Variable" },
  { "<leader>rlc", "<cmd>lua require('refactoring').debug.cleanup({})<CR>",                desc = "Print Cleanup" },
  { "<leader>e",   group = "Neotree" },
  { "<leader>e_",  hidden = true },
  { "<leader>g",   group = "[G]it" },
  { "<leader>g_",  hidden = true },
  { "<leader>s",   group = "[S]earch" },
  { "<leader>s_",  hidden = true },
  { "<leader>w",   group = "[W]orkspace" },
  { "<leader>w_",  hidden = true },
  { "leader>d",    group = "debug" },
  { "leader>d_",   hidden = true },
}

require('which-key').add {
  { "<leader>l",  group = "lsp" },
  { "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<cr>",      desc = "code action" },
  { "<leader>ld", "<cmd>lua vim.lsp.buf.definition()<cr>",       desc = "definition" },
  { "<leader>lh", "<cmd>lua vim.lsp.buf.hover()<cr>",            desc = "hover" },
  { "<leader>li", "<cmd>lua vim.lsp.buf.implementation()<cr>",   desc = "implementation" },
  { "<leader>lr", "<cmd>lua vim.lsp.buf.references()<cr>",       desc = "references" },
  { "<leader>ls", "<cmd>lua vim.lsp.buf.signature_help()<cr>",   desc = "signature help" },
  { "<leader>lt", "<cmd>lua vim.lsp.buf.type_definition()<cr>",  desc = "type definition" },
  { "<leader>lw", "<cmd>lua vim.lsp.buf.document_symbol()<cr>",  desc = "document symbol" },
  { "<leader>lW", "<cmd>lua vim.lsp.buf.workspace_symbol()<cr>", desc = "workspace symbol" },
  { "<leader>lD", "<cmd>lua vim.lsp.buf.declaration()<cr>",      desc = "declaration" },
  { "<leader>lR", "<cmd>lua vim.lsp.buf.rename()<cr>",           desc = "rename" },
  { "<leader>lF", "<cmd>lua vim.lsp.buf.formatting()<cr>",       desc = "formatting" },
}

require('which-key').add {
  { "<leader>H",   group = "help/debug/conceal" },
  { "<leader>Hc",  group = "help/debug/conceal" },
  { "<leader>Hh",  "<cmd>:set conceallevel=1<cr>",                           desc = "hide/conceal" },
  { "<leader>Hs",  "<cmd>:set conceallevel=0<cr>",                           desc = "show/unconceal" },
  { "<leader>Ht",  group = "help/debug/conceal" },
  { "<leader>Htt", "<cmd>:lua vim.treesitter.inspect_tree()<cr>",            desc = "show tree" },
  { "<leader>Htc", "<cmd>:lua =vim.treesitter.get_captures_at_cursor()<cr>", desc = "show capture" },
  { "<leader>Htn", "<cmd>:lua =vim.treesitter.get_node():type()<cr>",        desc = "show node" },
}

require('which-key').add {
  { "<leader>h",  group = "harpoon" },
  { "<leader>h1", "<cmd> lua require('harpoon.ui').nav_file(1)<cr>",        desc = "file 1" },
  { "<leader>h2", "<cmd> lua require('harpoon.ui').nav_file(2)<cr>",        desc = "file 2" },
  { "<leader>h3", "<cmd> lua require('harpoon.ui').nav_file(3)<cr>",        desc = "file 3" },
  { "<leader>ha", "<cmd>lua require('harpoon.mark').add_file()<cr>",        desc = "add file" },
  { "<leader>hm", "<cmd>lua require('harpoon.ui').toggle_quick_menu()<cr>", desc = "harpoon menu" },
  { "<leader>hn", "<cmd>lua require('harpoon.ui').nav_next()<cr>",          desc = "next file" },
  { "<leader>hp", "<cmd>lua require('harpoon.ui').nav_prev()<cr>",          desc = "previous file" },
  { "<leader>hr", "<cmd>lua require('harpoon.mark').rm_file()<cr>",         desc = "remove file" },
}


vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

vim.keymap.set('n', '<leader>f', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sr', require('telescope.builtin').resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader>m', require('telescope.builtin').marks, { desc = 'Marks' })

-- Lua
vim.keymap.set("n", "<leader>tt", function() require("trouble").toggle() end)
vim.keymap.set("n", "<leader>tw", function() require("trouble").toggle("workspace_diagnostics") end)
vim.keymap.set("n", "<leader>tq", function() require("trouble").toggle("quickfix") end)
vim.keymap.set("n", "<leader>tl", function() require("trouble").toggle("loclist") end)
vim.keymap.set("n", "gR", function() require("trouble").toggle("lsp_references") end)

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })
