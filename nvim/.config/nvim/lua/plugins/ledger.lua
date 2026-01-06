return {
  -- Ledger syntax and helper commands
  {
    "ledger/vim-ledger",
    version = false,
    ft = "ledger",
    init = function()
      vim.g.ledger_bin = "hledger"
      vim.g.ledger_fuzzy_account_completion = 1
      vim.g.ledger_date_format = "%Y-%m-%d"
      vim.g.ledger_align_at = 70
    end,
  },

  -- Treesitter syntax highlighting for ledger files
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = { "ledger" },
    },
  },

  -- Completion: Blink + cmp-hledger source
  {
    "saghen/blink.cmp",
    optional = true,
    opts = {
      sources = {
        default = {
          "lsp",
          "path",
          "snippets",
          "buffer",
          "omni",
          "hledger", -- enable account name completion ✨
        },
        providers = {
          hledger = {
            name = "hledger",
            module = "cmp_hledger",
          },
        },
      },
    },
  },
  {
    "kirasok/cmp-hledger",
    ft = "ledger",
    dependencies = { "saghen/blink.cmp" },
  },

  -- Linting using hledger check
  {
    "mfussenegger/nvim-lint",
    ft = "ledger",
    opts = {
      events = { "BufWritePost", "BufReadPost", "InsertLeave" },
      linters_by_ft = {
        ledger = { "hledger" }, -- requires hledger in $PATH
      },
    },
  },

  -- Trim whitespace automatically
  {
    "stevearc/conform.nvim",
    ft = "ledger",
    opts = {
      formatters_by_ft = {
        ledger = { "trim_newlines", "trim_whitespace" },
      },
    },
  },
}
