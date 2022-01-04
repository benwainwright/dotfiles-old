local lsp_configure = require("lsp.lsp_configure")

lsp_configure.configure_servers {
  {
    name = "sumneko_lua",
    options = {
      settings = {
        Lua = {
          diagnostics = {
              globals = {'vim'},
          },
        }
      }
    }
  },
  {
    name = "tsserver",
    options = {
      debounce_text_changes = 150,
      on_attach = function(client, bufnr)
        local ts_utils = require("nvim-lsp-ts-utils")
        ts_utils.setup {
          eslint_enable_diagnostics = false,
          disable_commands = false,
          -- eslint_enable_disable_comments = true,
          enable_formatting = false,
          update_imports_on_move = true,
          require_confirmation_on_move = false,
          watch_dir = nil,
        }

        client.resolved_capabilities.document_formatting = false
        ts_utils.setup_client(client)
      end
    }
  },
  {
    name = "diagnosticls",
    options = {

      filetypes = {"typescript", "typescriptreact"},
      flags = {
        debounce_text_changes = 150,
      },
      init_options = {
        linters = {
          eslint = {
            command = 'eslint_d',
            rootPatterns = { '.eslintrc.js', '.eslintrc.json', 'package.json' },
            debounce = 100,
            args = { '--stdin', '--stdin-filename', '%filepath', '--format', 'json' },
            sourceName = 'eslint',
            parseJson = {
              errorsRoot = '[0].messages',
              line = 'line',
              column = 'column',
              endLine = 'endLine',
              endColumn = 'endColumn',
              message = '[eslint] ${message} [${ruleId}]',
              security = 'severity'
            },
            securities = {
              [2] = 'error',
              [1] = 'warning'
            }
          },
          markdownlint = {
            command = 'markdownlint',
            rootPatterns = { '.git' },
            isStderr = true,
            debounce = 100,
            args = { '--stdin' },
            offsetLine = 0,
            offsetColumn = 0,
            sourceName = 'markdownlint',
            securities = {
              undefined = 'hint'
            },
            formatLines = 1,
            formatPattern = {
              '^.*:(\\d+)\\s+(.*)$',
              {
                line = 1,
                column = -1,
                message = 2,
              }
            }
          }
        },
        filetypes = {
          javascript = 'eslint',
          javascriptreact = 'eslint',
          typescript = 'eslint',
          typescriptreact = 'eslint',
          markdown = 'markdownlint',
          pandoc = 'markdownlint'
        },
        formatters = {
          prettier = {
            args = { '--stdin', '--stdin-filepath', '%filepath' },

            rootPatterns = {
              '.prettierrc',
              '.prettierrc.json',
              '.prettierrc.toml',
              '.prettierrc.json',
              '.prettierrc.yml',
              '.prettierrc.yaml',
              '.prettierrc.json5',
              '.prettierrc.js',
              '.prettierrc.cjs',
              'prettier.config.js',
              'prettier.config.cjs',
            },
            command = 'prettier_d_slim'
          }
        },
        formatFiletypes = {
          css = 'prettier',
          javascript = 'prettier',
          javascriptreact = 'prettier',
          json = 'prettier',
          scss = 'prettier',
          typescript = 'prettier',
          typescriptreact = 'prettier'
        }
      }
    }
  },
  "bashls",
  "jedi_language_server",
  "vimls",
  "spectral",
  "ccls",
  "dockerls",
  "gopls",
  "html",
  "jsonls",
  "jdtls",
  "sqls",
  "tailwindcss",
  "yamlls",
}

