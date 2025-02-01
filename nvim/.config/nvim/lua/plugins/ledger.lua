return {

  "wllfaria/ledger.nvim",
  -- tree sitter needs to be loaded before ledger.nvim loads
  dependencies = { "nvim-treesitter/nvim-treesitter" },
  config = function()
    require("ledger").setup({
      extensions = {
        "ledger",
        "hledger",
        "journal",
      },
      completion = {
        cmp = { enabled = true },
      },
      snippets = {
        cmp = { enabled = true },
        luasnip = { enabled = false },
        native = { enabled = false },
      },
      keymaps = {
        snippets = {
          new_posting = { "tt" },
          new_account = { "acc" },
          new_posting_today = { "td" },
          new_commodity = { "cm" },
        },
        reports = {},
      },
      diagnostics = {
        lsp_diagnostics = true,
        strict = false,
      },
    })
  end,
}
