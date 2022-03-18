local servers = require("lsp.servers")
local react_filter = require("lsp.react-filter")
local fs = require("lsp.fs")
local Delay = require("Delay")

servers.configure {
  {
    name = "sumneko_lua",
    options = {
      settings = {
        Lua = {
          diagnostics = {
            globals = {'vim'}
          } }
      }
    }
  },
  {
    name = "tsserver",
    options = {
      handlers = {
        ['textDocument/definition'] = react_filter
      },
      debounce_text_changes = 150,
      on_attach = function(client)
        local ts_utils = require("nvim-lsp-ts-utils")
        ts_utils.setup {
          eslint_enable_diagnostics = false,
          disable_commands = false,
          -- eslint_enable_disable_comments = true,
          enable_formatting = false,
          update_imports_on_move = true,
          require_confirmation_on_move = false,
          watch_dir = nil
        }

        client.resolved_capabilities.document_formatting = false
        ts_utils.setup_client(client)
      end
    }
  },
  {
    name = "efm",
    options = {
      init_options = {
        documentFormatting = true
      },
      filetypes = {'lua'},
      settings = {
        rootMarkers = {
          ".git/"
        },
        languages = {
          lua = {
            {
              formatCommand = "lua-format -i --config=lua-format.config",
              formatStdin = true
            }
          }
        }
      }
    }
  },
  "bashls",
  "jedi_language_server",
  "vimls",
  "ccls",
  "dockerls",
  "gopls",
  "html",
  -- "jsonls",
  "jdtls",
  "rust_analyzer",
  "sqls",
  "tailwindcss",
  "yamlls"
}

