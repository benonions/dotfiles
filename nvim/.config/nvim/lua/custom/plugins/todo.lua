return {
  "folke/todo-comments.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  opts = {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  },
  vim.api.nvim_set_keymap('n', '<leader>td', ':TodoTelescope<CR>', { noremap = true, silent = true })
}
