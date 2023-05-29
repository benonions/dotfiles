lvim.format_on_save.enabled = true
lvim.colorscheme = "tokyonight-moon"
lvim.transparent_window = true
lvim.relativenumber = true
-- to disable icons and use a minimalist setup, uncomment the following
-- lvim.use_icons = false

-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = "space"
vim.g.maplocalleader = ","

lvim.builtin.which_key.mappings["t"] = {
  name = "Diagnostics",
  t = { "<cmd>TroubleToggle<cr>", "trouble" },
  w = { "<cmd>TroubleToggle workspace_diagnostics<cr>", "workspace" },
  d = { "<cmd>TroubleToggle document_diagnostics<cr>", "document" },
  q = { "<cmd>TroubleToggle quickfix<cr>", "quickfix" },
  l = { "<cmd>TroubleToggle loclist<cr>", "loclist" },
  r = { "<cmd>TroubleToggle lsp_references<cr>", "references" },
}

lvim.builtin.which_key.mappings["n"] = {
  name = "Neorg",
  i = { "<cmd>Neorg index<cr>", "index" },
  j = { "<cmd>Neorg journal<cr>", "journal" },
  t = { "<cmd>Neorg journal today<cr>", "today" },
}


-- Change Telescope navigation to use j and k for navigation and n and p for history in both input and normal mode.
-- we use protected-mode (pcall) just in case the plugin wasn't loaded yet.
local _, actions = pcall(require, "telescope.actions")
lvim.builtin.telescope.defaults.mappings = {
  -- for input mode
  i = {
    ["<C-j>"] = actions.move_selection_next,
    ["<C-k>"] = actions.move_selection_previous,
    ["<C-n>"] = actions.cycle_history_next,
    ["<C-p>"] = actions.cycle_history_prev,
  },
  -- for normal mode
  n = {
    ["<C-j>"] = actions.move_selection_next,
    ["<C-k>"] = actions.move_selection_previous,
  },
}


-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.setup.renderer.icons.show.git = false
lvim.builtin.bufferline.active = false
-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = {
  "bash",
  "c",
  "javascript",
  "json",
  "lua",
  "python",
  "typescript",
  "tsx",
  "css",
  "rust",
  "java",
  "yaml",
  "org",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enable = true


lvim.builtin.dap.active = true
local dap = require("dap")
dap.adapters.go = {
  type = "server",
  port = 2345,
}

-- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
dap.configurations.go = {
  {
    type = "go",
    name = "delve devspace debug",
    request = "attach",
    mode = "remote",
    substitutepath = { {
      from = "${workspaceFolder}",
      to = "/app"
    } }
  }
}

-- lvim.lang.terraform.formatters = { { exe = 'terraform_fmt' } }


-- Additional Plugins
lvim.plugins = {
  -- {
  --   'codota/tabnine-nvim',
  --   build = "./dl_binaries.sh",
  --   require('tabnine').setup({
  --     disable_auto_comment = false,
  --     accept_keymap = "<Tab>",
  --     dismiss_keymap = "<C-]>",
  --     debounce_ms = 800,
  --     suggestion_color = { gui = "#808080", cterm = 244 },
  --     exclude_filetypes = { "TelescopePrompt" }
  --   })
  -- },
  {
    "folke/trouble.nvim",
    cmd = "TroubleToggle",
  },
  {
    "miversen33/netman.nvim",
    config = function()
      require("netman")
    end,
  },
  {
    "folke/todo-comments.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    config = function()
      require("todo-comments").setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      }
    end
  },
  {
    "tpope/vim-fugitive",
    cmd = {
      "G",
      "Git",
      "Gdiffsplit",
      "Gread",
      "Gwrite",
      "Ggrep",
      "GMove",
      "GDelete",
      "GBrowse",
      "GRemove",
      "GRename",
      "Glgrep",
      "Gedit"
    },
    ft = { "fugitive" }
  },
  {
    "simrat39/symbols-outline.nvim",
    config = function()
      require('symbols-outline').setup()
    end
  },
  {
    "folke/zen-mode.nvim"
  },
  {
    "nvim-neorg/neorg",
    build = ":Neor sync-parsers",
    opts = {
      load = {
        ["core.defaults"] = {},  -- Loads default behaviour
        ["core.concealer"] = {}, -- Adds pretty icons to your documents
        ["core.itero"] = {},
        ["core.dirman"] = {      -- Manages Neorg workspaces
          config = {
            workspaces = {
              notes = "~/notes"
            },
            default_workspace = "notes"
          }
        },
        ["core.export"] = {},
        ["core.export.markdown"] = {},
        ["core.presenter"] = {
          config = {
            zen_mode = "zen-mode"
          }
        },
        ["core.summary"] = {},
        ["core.qol.todo_items"] = {
          config = {
            create_todo_items = true,
            create_todo_parents = true,
            order = "undone",
            order_with_children = "undone",
          }
        },
        ["core.integrations.telescope"] = {},
      }
    },
    dependencies = { { "nvim-lua/plenary.nvim" }, { "nvim-neorg/neorg-telescope" } },
  }
}
