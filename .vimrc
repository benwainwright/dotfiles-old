" Plugins
"==============

" Install vim-plug if it is missing
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged') 
Plug 'vim-scripts/git-log'
Plug 'kien/rainbow_parentheses.vim'
Plug 'junegunn/fzf.vim'
Plug '/usr/local/opt/fzf'
Plug 'airblade/vim-gitgutter'
Plug 'flazz/vim-colorschemes'
Plug 'tpope/vim-cucumber'
Plug 'tpope/vim-surround'
Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-commentary'
Plug 'altercation/vim-colors-solarized'
Plug 'easymotion/vim-easymotion'
Plug 'aklt/plantuml-syntax'
Plug 'Shougo/deoplete.nvim'
Plug 'carlitux/deoplete-ternjs'
Plug 'mustache/vim-mustache-handlebars'
Plug 'scrooloose/nerdtree'
Plug 'ternjs/tern_for_vim'
Plug 'vim-scripts/AutoComplPop'
Plug 'jacoborus/tender.vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'junkblocker/patchreview-vim'
Plug 'janko-m/vim-test'
Plug 'w0rp/ale'
Plug 'gioele/vim-autoswap'
Plug 'ekalinin/Dockerfile.vim'
Plug 'vim-airline/vim-airline'
Plug 'tpope/vim-dispatch'
Plug 'junegunn/gv.vim'
Plug 'arkwright/vim-whiplash'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'martinda/Jenkinsfile-vim-syntax'
Plug 'tpope/vim-unimpaired'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
call plug#end()

" Core VIM settings
"==============
set clipboard=unnamed           " Allows the vim unknown buffer to work with the system clipboard
set encoding=UTF-8              " Set character encoding used inside VIM
set hidden                      " Change buffer without saving

set tabstop=2                   " Number of spaces that a tab counts for
set expandtab                   " Use appropriate number of spaces to insert a tab
set shiftwidth=2                " Number of spaces used for indentenation
set shiftround                  " << >> snap to multiples of shiftwidth

set hlsearch                    " highlight search hits
set incsearch                   " Update search matches as characters are added to search
set ignorecase                  " Ignore case in search patterns
set smartcase                   " If search pattern contains uppercase chars, ignorecase is turned off

set nocompatible                " set 'nocompatible' to ward off unexpected things that your 
                                " distro might have made, as well as sanely reset options when 
                                " re-sourcing .vimrc
                                
set wildmode=longest,list,full  " Completion mode - see :help wildmode for more
                                " info on specific settings
set wildmenu                    " Enhanced command line completion mode
set showcmd                     " Show last command at the bottom of the screen
set laststatus=2                " Always show statusline
set ruler                       " Show line and column number
set number relativenumber       " Turn on relative line numbering
set cursorline                  " Highlight current line
set splitright                  " vertical split opens on the right

set directory^=$HOME/.vim/tmp/  " Where to store temporary files
set autoread                    " If file changes on disk and buffer hasn't
                                " changed, autoread from disk
set undofile                    " Store undo data between sessions
set undodir=~/.vim/undo         " Location of undo data

" When entering a buffer, turn on relative number, turn it off when leaving
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

set confirm                     " Actual confirm box rather than dialog box on errors (such as if
                                " you quit without write

syntax enable                   " Enable syntax highlighting
filetype indent plugin on       " attempt to determine the type of a file based on its name and possibly its
                                " contents. Use this to allow intelligent auto-indenting for each filetype,
                                " and for plugins that are filetype specific.

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0 

let g:ale_linters = {'javascript': ['eslint']}
let g:ale_javascript_eslint_use_global = 1
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1
" If the plugged directory hasn't been created, install all plugins
if empty(glob('~/.vim/plugged'))
  PlugInstall
endif

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

colorscheme Gruvbox
set background=dark
let g:airline_section_b = '%{fugitive#head()}'

 " Ripped from https://github.com/junegunn/fzf.vim/issues/603#issuecomment-374669057
function! s:open_branch_fzf(line)
  let l:parser = split(a:line)
  let l:branch = l:parser[0]
  if l:branch ==? '*'
    let l:branch = l:parser[1]
  endif
  execute '!git checkout ' . l:branch
endfunction

function! s:open_files_in_dir(dir)
  execute 'cd ' . a:dir
  GitFiles
endfunction

nnoremap <leader>cd :call fzf#run({
			\ 'sink': function('s:open_files_in_dir'),
			\ 'source': 'ls',
			\ 'dir': '~/workspace/',
			\ 'down': '40%'})<CR>

command! -bang -nargs=0 GCheckout
  \ call fzf#vim#grep(
  \   'git branch -v', 0,
  \   {
  \     'sink': function('s:open_branch_fzf')
  \   },
  \   <bang>0
  \ )

" Map leader key to be the space bar
let mapleader = "\<Space>"

let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

let test#strategy = "vimux"

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" Insert mode mappings
inoremap <c-u> <esc>gUawi

" Tern mappings
nnoremap <leader>td :TernDef<CR>
nnoremap <leader>tdp :TernDefPreview<CR>
nnoremap <leader>tds :TernDefSplit<CR>
nnoremap <leader>tdt :TernDefTab<CR>
nnoremap <leader>tr :TernRefs<CR>
nnoremap <leader>trn :TernRename<CR>
nnoremap <leader>tt :TernType<CR>

" Mapping selecting mappings
nnoremap <leader><tab> <plug>(fzf-maps-n)
xnoremap <leader><tab> <plug>(fzf-maps-x)
onoremap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
inoremap <c-x><c-k> <plug>(fzf-complete-word)
inoremap <c-x><c-f> <plug>(fzf-complete-path)
inoremap <c-x><c-j> <plug>(fzf-complete-file-ag)
inoremap <c-x><c-l> <plug>(fzf-complete-line)

nnoremap <silent> t<C-n> :TestNearest<CR> " t Ctrl+n
nnoremap <silent> t<C-f> :TestFile<CR>    " t Ctrl+f
nnoremap <silent> t<C-s> :TestSuite<CR>   " t Ctrl+s
nnoremap <silent> t<C-l> :TestLast<CR>    " t Ctrl+l
nnoremap <silent> t<C-g> :TestVisit<CR>   " t Ctrl+g

nnoremap <C-n> :bprevious<CR>
nnoremap <C-p> :bnext<CR>

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})lug 'tpope/vim-surround'

" Map fzf commands
nnoremap <leader>k :Ag
nnoremap <leader>K :Ag!
nnoremap <leader>f :GitFiles
nnoremap <leader>F :GitFiles!
nnoremap <leader>g :GitFiles?
nnoremap <leader>b :Buffers
nnoremap <leader>c :Commits
nnoremap <leader>C :BCommits
nnoremap <leader>s :Gstatus
nnoremap <leader>S :Gcommit



noremap<C-s> :NERDTreeToggle<CR>

noremap<C-w> :Gblame<CR>

nnoremap - ddp
nnoremap _ ddkP

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

let g:GITLOG_default_mode = 2
noremap <silent> <f7> :call GITLOG_ToggleWindows()<cr>
noremap <silent> <f5> :call GITLOG_FlipWindows()<cr>

let g:ale_sign_error = '✗'
let g:ale_sign_warning = ''

let g:gitgutter_sign_added = ''
let g:gitgutter_sign_modified = ''
let g:gitgutter_sign_removed = ''
