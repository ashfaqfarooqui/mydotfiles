return {
  "yetone/avante.nvim",
  opts = {
    instruction_file = "agent.md",
    provider = "ollama",
    providers = {
      ollama = {
        endpoint = "http://localhost:11434",
        model = "codellama:13b",
        timeout = 30000,
      },
    },
  },
}
