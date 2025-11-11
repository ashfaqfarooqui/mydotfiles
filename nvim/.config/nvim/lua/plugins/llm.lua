return {
  "yetone/avante.nvim",
  opts = {
    instruction_file = "agent.md",
    provider = "ollama",
    providers = {
      ollama = {
        endpoint = "https://localhost:11434",
        model = "qwen3",
        timeout = 30000,
      },
    },
  },
}
