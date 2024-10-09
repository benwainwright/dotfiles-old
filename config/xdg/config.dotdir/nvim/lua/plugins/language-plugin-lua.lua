return {
    {"euclidianAce/BetterLua.vim", ft = "lua"}, {
        "folke/lazydev.nvim",
        ft = "lua",
        dependencies = {'hrsh7th/nvim-cmp', "Bilal2453/luvit-meta"},
        opts = {library = {{path = "luvit-meta/library", words = {"vim%.uv"}}}}
    }
}
