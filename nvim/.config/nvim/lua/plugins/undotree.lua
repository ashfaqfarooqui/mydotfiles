return {
  "jiaoshijie/undotree",
  dependencies = "nvim-lua/plenary.nvim",
  config = true,
  enabled = true,
  keys = { -- load the plugin only when using it's keybinding:
    { "<leader>U", "<cmd>lua require('undotree').toggle()<cr>", desc = "Undo tree" },
  },
}
