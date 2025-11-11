return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim", -- optional, for diff integration
    },
    cmd = "Neogit", -- lazy load when the command is run
    config = function()
      local neogit = require("neogit")
      neogit.setup({
        -- your preferred options
        integrations = { diffview = true },
      })
      vim.keymap.set("n", "<leader>gg", function()
        neogit.open({ cwd = vim.fn.getcwd() })
      end, { desc = "Neogit (cwd)" })
    end,
  },
}
