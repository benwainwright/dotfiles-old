local lsp_configure = require("lsp.lsp_configure")
local lsp_on_attach = require('lsp.lsp_on_attach')

lsp_configure.server("bashls")
lsp_configure.server("jedi_language_server")
lsp_configure.server("vimls")
lsp_configure.server("spectral")
lsp_configure.server("ccls")
lsp_configure.server("cssls")
lsp_configure.server("dockerls")
lsp_configure.server("gopls")
lsp_configure.server("html")
lsp_configure.server("jsonls")
lsp_configure.server("jdtls")
lsp_configure.server("sqls")
lsp_configure.server("tailwindcss")
lsp_configure.server("yamlls")

lsp_configure.server("sumneko_lua", {
  on_attach = lsp_on_attach.global_lsp_config,
  settings = {
    Lua = {
      diagnostics = {
          globals = {'vim'},
      },
    }
  }
})

lsp_configure.server("tsserver", {
  debounce_text_changes = 150,
  on_attach = function(client, bufnr)
    local ts_utils = require("nvim-lsp-ts-utils")
    ts_utils.setup {
      eslint_enable_diagnostics = false,
      disable_commands = false,
      enable_formatting = false,
      update_imports_on_move = true,
      require_confirmation_on_move = false,
      watch_dir = nil,
    }

    client.resolved_capabilities.document_formatting = false
    lsp_on_attach.global_lsp_config(client, bufnr)
    ts_utils.setup_client(client)
  end
})

lsp_configure.server("diagnosticls", {
  on_attach = lsp_on_attach.configure_lsp,
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
})
