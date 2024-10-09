return {
    {"HiPhish/rainbow-delimiters.nvim", event = "VeryLazy"}, {
        "hoob3rt/lualine.nvim",
        lazy = false,
        dependencies = {"nvim-tree/nvim-web-devicons"},
        opts = {
            options = {
                section_separators = {"", ""},
                component_separators = {"", ""}
            }
        }
    }, "kshenoy/vim-signature", "ryanoasis/vim-devicons", {
        "rcarriga/nvim-notify",
        opts = {renderer = "simple"},
        config = function() vim.notify = require('notify') end
    }, {
        "gelguy/wilder.nvim",
        event = 'VeryLazy',
        dependencies = {'ryanoasis/vim-devicons'},
        config = function()
            -- local wilder = require('wilder')

            -- wilder.set_option('renderer', wilder.popupmenu_renderer({
            --     highlighter = wilder.basic_highlighter(),
            --     left = {' ', wilder.popupmenu_devicons()},
            --     right = {' ', wilder.popupmenu_scrollbar()}
            -- }))

            -- wilder.setup({modes = {":", "/", "?"}})
        end
    }
}
