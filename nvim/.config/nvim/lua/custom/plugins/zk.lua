return {
  "zk-org/zk-nvim",
  config = function()
    require("zk").setup({
      -- can be "telescope", "fzf", "fzf_lua", "minipick", or "select" (`vim.ui.select`)
      -- it's recommended to use "telescope", "fzf", "fzf_lua", or "minipick"
      picker = "telescope",

      lsp = {
        config = {
          cmd = { "zk", "lsp" },
          name = "zk",
        },

        -- automatically attach buffers in a zk notebook that match the given filetypes
        auto_attach = {
          enabled = true,
          filetypes = { "markdown" },
        },
      },
    })
  end
}
