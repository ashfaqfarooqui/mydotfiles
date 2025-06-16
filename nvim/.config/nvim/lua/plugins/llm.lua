return {
  "yetone/avante.nvim",
  event = "VeryLazy",
  version = false, -- Never set this value to "*"! Never!
  opts = {
    -- add any opts here
    system_prompt = function()
      local hub = require("mcphub").get_hub_instance()
      return hub and hub:get_active_servers_prompt() or ""
    end,
    -- Using function prevents requiring mcphub before it's loaded
    custom_tools = function()
      return {
        require("mcphub.extensions.avante").mcp_tool(),
      }
    end,
    -- for example
    provider = "ollama",
    cursor_applying_provider = "ollama",
    behabiour = {
      enable_cursor_planning_mode = true,
    },
    providers = {
      ollama = {
        endpoint = "http://127.0.0.1:11434",
        -- model = "llama3.3:latest", -- your desired model (or use gpt-4o, etc.)
        model = "qwen3:32b",
        extra_request_body = {
          timeout = 30000, -- Timeout in milliseconds, increase this for reasoning models
          temperature = 0.75,
          max_completion_tokens = 328192, -- Increase this to include reasoning tokens (for reasoning models)
          reasoning_effort = "medium", -- low|medium|high, only used for reasoning models
        },
      },
    },
  },
  -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
  build = "make",
  -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "nvim-lua/plenary.nvim",
    "MunifTanjim/nui.nvim",
    --- The below dependencies are optional,
    "echasnovski/mini.pick", -- for file_selector provider mini.pick
    "nvim-telescope/telescope.nvim", -- for file_selector provider telescope
    "hrsh7th/nvim-cmp", -- autocompletion for avante commands and mentions
    "ibhagwan/fzf-lua", -- for file_selector provider fzf
    "stevearc/dressing.nvim", -- for input provider dressing
    "folke/snacks.nvim", -- for input provider snacks
    "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
    "zbirenbaum/copilot.lua", -- for providers='copilot'
    {
      -- support for image pasting
      "HakonHarnes/img-clip.nvim",
      event = "VeryLazy",
      opts = {
        -- recommended settings
        default = {
          embed_image_as_base64 = false,
          prompt_for_file_name = false,
          drag_and_drop = {
            insert_mode = true,
          },
          -- required for Windows users
          use_absolute_path = true,
        },
      },
    },
    {
      -- Make sure to set this up properly if you have lazy=true
      "MeanderingProgrammer/render-markdown.nvim",
      opts = {
        file_types = { "markdown", "Avante" },
      },
      ft = { "markdown", "Avante" },
    },
  },
}
-- return {
--  "huggingface/llm.nvim",
--  enabled = true,
--   event = "VeryLazy",
--   keys = {
--     {
--       "<c-j>",
--       function()
--         require("llm.completion").complete()
--       end,
--       mode = "i",
--       desc = "complete",
--     },
--   },
--   opts = {
--     lsp = {
--       bin_path = vim.api.nvim_call_function("stdpath", { "data" }) .. "/mason/bin/llm-ls",
--     },
--
--     backend = "ollama",
--     model = "qwen2.5-coder:32b",
--     url = "http://127.0.0.1:11434", -- llm-ls uses "/api/generate"
--     -- cf https://github.com/ollama/ollama/blob/main/docs/api.md#parameters
--     fim = {
--       enabled = true,
--       prefix = "<｜fim▁begin｜>",
--       suffix = "<｜fim▁hole｜>",
--       middle = "<｜fim▁end｜>",
--     },
--   },
-- }
