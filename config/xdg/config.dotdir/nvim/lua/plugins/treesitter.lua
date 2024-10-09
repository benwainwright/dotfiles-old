return {
    'nvim-treesitter/nvim-treesitter',
    event = 'VeryLazy',
    build = function()
        require("nvim-treesitter.install").update({with_sync = true})()
    end,


    config = function()
        local ts = require 'nvim-treesitter.configs'
        ts.setup {
            ensure_installed = {
                'javascript', 'typescript', 'bash', 'css', 'html', 'jsdoc',
                'json', 'json5', 'jsonc', 'lua', 'make', 'python', 'regex',
                'scss', 'svelte', 'tsx', 'vim', 'yaml', 'go'
            },
            highlight = {enable = true}
        }
    end
}
