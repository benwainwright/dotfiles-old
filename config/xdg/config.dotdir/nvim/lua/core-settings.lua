local nvim = require("nvim-api")
local options = nvim.options
local exec = nvim.exec

-- Delete comment characters when joining lines
options.append("formatoptions", "j")

-- Increase the undo limit
options.set("history", 1000)

-- Set relative line numbering
options.set("relativenumber", true)

-- Enable syntax highlighting
exec("syntax enable")

-- Indicates a fast terminal connection
options.set("ttyfast", true)

-- Consider binary and hex strings to be numbers
options.set("nrformats", "bin,hex")

-- attempt to determine the type of a file based on its name and possibly its
-- contents. Use this to allow intelligent auto-indenting for each filetype
-- and for plugins that are filetype specific.
exec("filetype indent plugin on")

-- Actual confirm box rather than dialog box on errors (such as if
-- you quit without writing to a file)
options.set("confirm", true)

-- Number of milliseconds before swap file is written to disk
options.set("updatetime", 300)

-- Allows the vim unknown buffer to work with the system clipboard, so if you
-- yank a piece of text, it will be available to paste outside of VIM
options.set("clipboard", "unnamed")

-- How characters are represented by NVIM internally
options.set("encoding", "UTF-8")

-- Default character encoding for a new buffer
exec("setglobal fileencoding=utf-8")

-- The buffer local setting 'fileencoding' setting specifies
-- the encoding of a given buffer. This list will be used
-- in order to decide what to set it to
options.set("fileencodings", "ucs-bom,utf-8,latin1")

-- Without this setting, when I switch to a new buffer, the current
-- one is unloaded. This means that if I've made a change I'll then
-- be asked if I want to save it. With this turned on, the buffer
-- simply becomes 'hidden'
options.set("hidden", true)

-- Don't redraw the screen while executing macros, registers and other
-- untyped commands
options.set("lazyredraw", true)

-- All horizontal splits go to the bottom half
options.set("splitbelow", true)

-- Number of spaces that tabs count for
options.set("tabstop", 2)

-- Number of spaces used for indentation
options.set("shiftwidth", 2)

-- In insert mode, use the appropriate number of spaces to insert a tab
options.set("expandtab", true)

-- Indenting (so << and >>) is rounded to multiples of 'shiftwidth'
options.set("shiftround", true)

-- Use marker comments for code folding
options.set("foldmethod", "marker")

-- Highlight search hits
options.set("hlsearch", true)

-- Update search matches as characters are added to search
options.set("incsearch", true)

-- Ignore case in search patterns
options.set("ignorecase", true)

-- Override 'ignorecase' if the search pattern contains
-- uppercase characters
options.set("smartcase", true)

-- Completion mode in command line:
-- * longest: Complete till ongest common string
-- * list: When more than one match, list all matches
-- * full: Complete the next full match
options.set("wildmode", "longest,list,full")

-- Enhanced command mode completion
options.set("wildmenu", true)

-- Show last command used below the status bar
options.set("showcmd", true)

-- Always draw the signcolumn
options.set("signcolumn", "yes")

-- Always draw the statusline regardless of the number of windows
options.set("laststatus", 2)

-- Show line and column numbers
options.set("ruler", true)

-- Vertical splits always open on the right
options.set("splitright", true)

-- If a file changes on disk and the buffer hasn't changed
-- autoread from disk
options.set("autoread", true)

-- Write to disk automatically after certain commands
-- are executed
options.set("autowrite", true)

-- Store undo data in between sessions
options.set("undofile", true)

-- Add these characters to VIM's definition of what a 'word'
-- is for the purposes of motions
options.append("iskeyword", ",_,$,@,%,-")

-- Completion options
-- * menu: Use a popup menu to show the possible completions
-- * menuone: Use the popup menu also when there is only one match
-- * preview: Show extra information about the selected completion
-- * noselect: Do not select a match in the menu. Force the user to choose
options.append("completeopt", ",menuone,noinsert,preview,noselect")


-- Don't show 'ins-completion-menu' messages, e.g. "match 1 of 2"
-- "the only match" are not shown
options.append("shortmess", "c")

-- Turns off the bell in specific situations. In this case
-- when there is an unknown char after <C-G> in Insert mode
options.append("belloff", ",ctrlg")

-- How to display hidden characters
options.set("listchars", "tab:→ ,space:·,nbsp:␣,trail:•,eol:¶,precedes:«,extends:»")

