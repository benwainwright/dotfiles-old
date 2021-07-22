local lspConfig = require('lspconfig')
local lspInstall = require('lspinstall')

local on_attach = function(client, bufnr)

  require "lsp_signature".on_attach()  -- Note: add in lsp client on-attach
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', '<space>jD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', '<space>jd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua require(\'lspsaga.hover\').render_hover_doc()<CR>', opts)
  buf_set_keymap('n', '<space>ji', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>r', '<cmd>lua require(\'lspsaga.rename\').rename()<CR>', opts)
  buf_set_keymap('n', '<space>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('x', '<space>a', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', opts)
  buf_set_keymap('n', '<space>jr', '<cmd>LspTrouble lsp_references<cr>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>f", "<cmd>lua require'lspsaga.provider'.lsp_finder()<CR>", opts)

  vim.fn.sign_define("LspDiagnosticsSignError",
  {text = ""})
  vim.fn.sign_define("LspDiagnosticsSignWarning",
  {text = ""})
  vim.fn.sign_define("LspDiagnosticsSignInformation",
  {text = ""})
  vim.fn.sign_define("LspDiagnosticsSignHint",
  {text = ""})

  vim.lsp.handlers['textDocument/codeAction'] = require'lsputil.codeAction'.code_action_handler
  vim.lsp.handlers['textDocument/references'] = require'lsputil.locations'.references_handler
  vim.lsp.handlers['textDocument/definition'] = require'lsputil.locations'.definition_handler
  vim.lsp.handlers['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
  vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
  vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
  vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
  vim.lsp.handlers["workspace/symbol"] = require'fzf_lsp'.workspace_symbol_handler
  vim.lsp.handlers["textDocument/formatting"] = nil

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = false,
      underline = true,
      signs = true,
    }
  )


  vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]]

  require('lspkind').init({
    --
    with_text = false,

    -- default symbol map
    -- can be either 'default' or
    -- 'codicons' for codicon preset (requires vscode-codicons font installed)
    --
    -- default: 'default'
    preset = 'codicons',

    -- override preset symbols
    --
    -- default: {}
    symbol_map = {
      Text = '',
      Method = 'ƒ',
      Function = '',
      Constructor = '',
      Variable = '',
      Class = '',
      Interface = 'ﰮ',
      Module = '',
      Property = '',
      Unit = '',
      Value = '',
      Enum = '了',
      Keyword = '',
      Snippet = '﬌',
      Color = '',
      File = '',
      Folder = '',
      EnumMember = '',
      Constant = '',
      Struct = ''
    },
  })
end

local lspInstallServers = {
  'bash',
  'vim',
  'graphql',
  'typescript',
  'diagnosticls',
  'dockerfile',
  'go',
  'yaml',
  'python',
  'css',
  'json',
  --'lua'
}

for _, server in ipairs(lspInstallServers) do
  if not lspInstall.is_server_installed(server) then
    lspInstall.install_server(server)
  end
end

lspInstall.setup()

local servers = lspInstall.installed_servers()

for _, server in pairs(servers) do
  if server == "tsserver"then

    lspConfig.tsserver.setup {
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

        -- required to fix code action ranges
        client.resolved_capabilities.document_formatting = false
        ts_utils.setup_client(client)


        on_attach(client, bufnr)
      end
    }

  elseif server == "diagnosticls" then

    lspConfig.diagnosticls.setup {
      on_attach = on_attach,
      filetypes = {"typescript", "typescriptreact"},
      flags = {
        debounce_text_changes = 150,
      },
      init_options = {
        linters = {
          eslint = {
            command = './node_modules/.bin/eslint',
            rootPatterns = { '.git' },
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
              '.eslintrc',
              '.eslintrc.cjs',
              '.eslintrc.js',
              '.eslintrc.json',
              '.eslintrc.yaml',
              '.eslintrc.yml',
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
  else
    lspConfig[server].setup {
      on_attach = on_attach,
    }
  end
end

