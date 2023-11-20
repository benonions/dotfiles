return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
    "MunifTanjim/nui.nvim",
    -- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
  },

  config = function()
    require('neo-tree').setup({
      close_if_last_window = true,
      follow_current_file = {
        enabled = true,         -- This will find and focus the file in the active buffer every time
        --               -- the current file is changed while the tree is open.
        leave_dirs_open = true, -- `false` closes auto expanded dirs, such as with `:Neotree reveal`
      },
    })
  end,

  vim.api.nvim_set_keymap('n', '<leader>e', ':Neotree toggle position=current<CR>', { noremap = true, silent = true })
}
