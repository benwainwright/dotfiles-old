local lspConfig = require('lspconfig')
local lspInstall = require('lspinstall')

local on_attach = function(client, bufnr)

  require "lsp_signature".on_attach()  -- Note: add in lsp client on-attach
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', '<space>jD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', '<space>jd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<space>ji', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>r', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('x', '<space>a', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', opts)
  buf_set_keymap('n', '<space>jr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({ border = 'single' })<CR>", opts)
  buf_set_keymap('n', '[d', "<cmd>lua vim.lsp.diagnostic.goto_prev({ popup_opts = { border = 'single' } })<CR>", opts)
  buf_set_keymap('n', ']d', "<cmd>lua vim.lsp.diagnostic.goto_next({ popup_opts = { border = 'single' } })<CR>", opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>f", "<cmd>lua require'lspsaga.provider'.lsp_finder()<CR>", opts)
  buf_set_keymap('n', "<space>S", "<cmd>lua require'telescope.builtin'.lsp_dynamic_workspace_symbols{}<CR>", opts)

  vim.fn.sign_define("DiagnosticSignError",
  {text = ""})
  vim.fn.sign_define("DiagnosticSignWarning",
  {text = ""})
  vim.fn.sign_define("DiagnosticSignInformation",
  {text = ""})
  vim.fn.sign_define("DiagnosticSignHint",
  {text = ""})

  vim.lsp.handlers['textDocument/codeAction'] = require'lsputil.codeAction'.code_action_handler
  vim.lsp.handlers['textDocument/references'] = require'lsputil.locations'.references_handler
  vim.lsp.handlers['textDocument/definition'] = require'lsputil.locations'.definition_handler
  vim.lsp.handlers['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
  vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
  vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
  vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
  vim.lsp.handlers["workspace/symbol"] = require'fzf_lsp'.workspace_symbol_handler

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = false,
      underline = true,
      signs = true,
    }
  )

  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
    vim.lsp.handlers.hover, {
      border = "single"
    }
  )

  vim.cmd [[
    augroup lsp
      autocmd!
      autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
      autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()
    augroup END
  ]]

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
  if server == "typescript" then

    lspConfig.typescript.setup {
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
        on_attach(client, bufnr)
        ts_utils.setup_client(client)
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
  else
    lspConfig[server].setup {
      capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
      on_attach = on_attach,
    }
  end
end

