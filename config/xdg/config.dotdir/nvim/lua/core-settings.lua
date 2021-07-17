local nvim = require("nvim-api")
local exec = nvim.exec


-- Delete comment characters when joining lines
vim.o.formatoptions = vim.o.formatoptions .. "j"

-- Increase the undo limit
vim.o.history = 1000

-- Enable the cursor line
vim.o.cursorline = true

-- Set relative line numbering
vim.o.relativenumber = true

-- Enable syntax highlighting
exec("syntax enable")

-- Indicates a fast terminal connection
vim.o.ttyfast = true

-- Consider binary and hex strings to be numbers
vim.o.nrformats = "bin,hex"

-- attempt to determine the type of a file based on its name and possibly its
-- contents. Use this to allow intelligent auto-indenting for each filetype
-- and for plugins that are filetype specific.
exec("filetype indent plugin on")

-- Actual confirm box rather than dialog box on errors (such as if
-- you quit without writing to a file)
vim.o.confirm = true

-- Number of milliseconds before swap file is written to disk
vim.o.updatetime = 300

-- Allows the vim unknown buffer to work with the system clipboard, so if you
-- yank a piece of text, it will be available to paste outside of VIM
vim.o.clipboard = "unnamed"

-- How characters are represented by NVIM internally
vim.o.encoding = "UTF-8"

-- Default character encoding for a new buffer
exec("setglobal fileencoding=utf-8")

-- The buffer local setting 'fileencoding' setting specifies
-- the encoding of a given buffer. This list will be used
-- in order to decide what to set it to
vim.o.fileencodings = "ucs-bom,utf-8,latin1"

-- Without this setting, when I switch to a new buffer, the current
-- one is unloaded. This means that if I've made a change I'll then
-- be asked if I want to save it. With this turned on, the buffer
-- simply becomes 'hidden'
vim.o.hidden = true

-- Don't redraw the screen while executing macros, registers and other
-- untyped commands
vim.o.lazyredraw = true

-- All horizontal splits go to the bottom half
vim.o.splitbelow = true

-- Number of spaces that tabs count for
vim.o.tabstop = 2

-- Number of spaces used for indentation
vim.o.shiftwidth = 2

-- In insert mode, use the appropriate number of spaces to insert a tab
vim.o.expandtab = true

-- Indenting (so << and >>) is rounded to multiples of 'shiftwidth'
vim.o.shiftround = true

-- Use marker comments for code folding
vim.o.foldmethod = "marker"

-- Highlight search hits
vim.o.hlsearch = true

-- Update search matches as characters are added to search
vim.o.incsearch = true

-- Ignore case in search patterns
vim.o.ignorecase = true

-- Override 'ignorecase' if the search pattern contains
-- uppercase characters
vim.o.smartcase = true

-- Completion mode in command line:
-- * longest: Complete till ongest common string
-- * list: When more than one match, list all matches
-- * full: Complete the next full match
vim.o.wildmode = "longest,list,full"

-- Enhanced command mode completion
vim.o.wildmenu = true

-- Show last command used below the status bar
vim.o.showcmd = true

-- Always draw the signcolumn
vim.o.signcolumn = "yes"

-- Always draw the statusline regardless of the number of windows
vim.o.laststatus = 2

-- Show line and column numbers
vim.o.ruler = true

-- Vertical splits always open on the right
vim.o.splitright = true

-- If a file changes on disk and the buffer hasn't changed
-- autoread from disk
vim.o.autoread = true

-- Write to disk automatically after certain commands
-- are executed
vim.o.autowrite = true

-- Store undo data in between sessions
vim.o.undofile = true

-- Add these characters to VIM's definition of what a 'word'
-- is for the purposes of motions
vim.o.iskeyword = vim.o.iskeyword .. ",_,$,@,%,-"

-- Completion options
-- * menu: Use a popup menu to show the possible completions
-- * menuone: Use the popup menu also when there is only one match
-- * preview: Show extra information about the selected completion
-- * noselect: Do not select a match in the menu. Force the user to choose
vim.o.completeopt = vim.o.completeopt .. ",menuone,noinsert,preview,noselect"

-- Don't show 'ins-completion-menu' messages, e.g. "match 1 of 2"
-- "the only match" are not shown
vim.o.shortmess = vim.o.shortmess .. "c"

-- Turns off the bell in specific situations. In this case
-- when there is an unknown char after <C-G> in Insert mode
vim.o.belloff = vim.o.belloff .. ",ctrlg"

-- How to display hidden characters
vim.o.listchars = "tab:→ ,space:·,nbsp:␣,trail:•,eol:¶,precedes:«,extends:»"

-- Don't display tab line
vim.o.showtabline = 0
