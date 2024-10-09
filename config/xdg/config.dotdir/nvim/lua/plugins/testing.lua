return {
    {
        "nvim-neotest/neotest",
        dependencies = {
            "marilari88/neotest-vitest", "nvim-neotest/nvim-nio",
            "nvim-lua/plenary.nvim", "antoinemadec/FixCursorHold.nvim",
            "nvim-treesitter/nvim-treesitter"
        },

        config = function()
            require("neotest").setup({
                adapters = {require("neotest-vitest")},
                log_level = vim.log.levels.DEBUG
            })
        end
    }
}
