return {
    {"lewis6991/gitsigns.nvim", config = true}, {
        "NeogitOrg/neogit",
        keys = {
            {
                "<leader>go",
                function()
                    require('neogit').open({kind = 'vsplit'})
                end
            }
        },
        config = true,
        dependencies = {
            "nvim-lua/plenary.nvim", "sindrets/diffview.nvim",
            "ibhagwan/fzf-lua"
        }
    }
}
