-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
-- document existing key chains
require('which-key').register {
  ['<leader>l'] = { name = 'lsp', _ = 'which_key_ignore' },
  ['<leader>g'] = { name = '[G]it', _ = 'which_key_ignore' },
  ['<leader>s'] = { name = '[S]earch', _ = 'which_key_ignore' },
  ['leader>d'] = { name = 'debug', _ = 'which_key_ignore' },
  ['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
  ['<leader>e'] = { name = 'Neotree', _ = 'which_key_ignore' },
  ['<leader>r'] = {
    name = "refactoring", -- group name
    r = { "<cmd>lua require('telescope').extensions.refactoring.refactors()<CR>", "Select refactor" },
    lf = { "<cmd>lua require('refactoring').debug.printf({ below = false })<CR>", "Print" },
    lv = { "<cmd>lua require('refactoring').debug.print_var()<CR>", "Print Variable" },
    lc = { "<cmd>lua require('refactoring').debug.cleanup({})<CR>", "Print Cleanup" },
  },
  ['<leader>h'] = {
    name = "harpoon",
    a = { "<cmd>lua require('harpoon.mark').add_file()<cr>", "add file" },
    r = { "<cmd>lua require('harpoon.mark').rm_file()<cr>", "remove file" },
    m = { "<cmd>lua require('harpoon.ui').toggle_quick_menu()<cr>", "harpoon menu" },
    n = { "<cmd>lua require('harpoon.ui').nav_next()<cr>", "next file" },
    p = { "<cmd>lua require('harpoon.ui').nav_prev()<cr>", "previous file" },
    ["1"] = { "<cmd> lua require('harpoon.ui').nav_file(1)<cr>", "file 1" },
    ["2"] = { "<cmd> lua require('harpoon.ui').nav_file(2)<cr>", "file 2" },
    ["3"] = { "<cmd> lua require('harpoon.ui').nav_file(3)<cr>", "file 3" },
  },
  ['<leader>H'] = {
    name = "help/debug/conceal",
    c = {
      name = "conceal",
      h = { ":set conceallevel=1<cr>", "hide/conceal" },
      s = { ":set conceallevel=0<cr>", "show/unconceal" },
    },
    t = {
      name = "treesitter",
      t = { vim.treesitter.inspect_tree, "show tree" },
      c = { ":=vim.treesitter.get_captures_at_cursor()<cr>", "show capture" },
      n = { ":=vim.treesitter.get_node():type()<cr>", "show node" },
    },
  },
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
