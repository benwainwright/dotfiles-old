set encoding=UTF-8

" Store swap files here
set directory^=$HOME/.vim/tmp//

" Indenting
set expandtab
set tabstop=2
set shiftwidth=2

" Highlight search hits
set hlsearch

" Search as chars are added to search
set incsearch

set laststatus=2

" Turn on syntax highlightingg
syntax enable

"set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible

set wildmenu

set showcmd

set ruler

" Actual confirm box rather than dialog box on errors (such as if you quit
" without write)
set confirm

set cursorline
" attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" " and for plugins that are filetype specific.
" filetype indent plugin on

set number relativenumber " Turn on relative line numbering

augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" Install vim-plug if it is missing
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0 

" Declare plugins
call plug#begin('~/.vim/plugged') 
Plug 'vim-scripts/git-log'
Plug 'kien/rainbow_parentheses.vim'
Plug 'junegunn/fzf.vim'
Plug '/usr/local/opt/fzf'
Plug 'airblade/vim-gitgutter'
Plug 'flazz/vim-colorschemes'
Plug 'tpope/vim-cucumber'
Plug 'christoomey/vim-tmux-navigator'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-commentary'
Plug 'altercation/vim-colors-solarized'
Plug 'easymotion/vim-easymotion'
Plug 'aklt/plantuml-syntax'
Plug 'Shougo/deoplete.nvim'
Plug 'mustache/vim-mustache-handlebars'
Plug 'scrooloose/nerdtree'
Plug 'ternjs/tern_for_vim'
Plug 'vim-scripts/AutoComplPop'
Plug 'jacoborus/tender.vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'junkblocker/patchreview-vim'
Plug 'codegram/vim-codereview'
Plug 'janko-m/vim-test'
Plug 'w0rp/ale'
Plug 'gioele/vim-autoswap'
Plug 'ekalinin/Dockerfile.vim'
Plug 'vim-airline/vim-airline'
Plug 'neomake/neomake'
call plug#end()

let g:ale_linters = {'javascript': ['eslint']}
let g:ale_javascript_eslint_use_global = 1
" If the plugged directory hasn't been created, install all plugins
if empty(glob('~/.vim/plugged'))
  PlugInstall
endif

colorscheme base16-atelierseaside
set background=dark

" Key mappings

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

" Tern mappings
nmap <leader>td :TernDef<CR>
nmap <leader>tdp :TernDefPreview<CR>
nmap <leader>tds :TernDefSplit<CR>
nmap <leader>tdt :TernDefTab<CR>
nmap <leader>tr :TernRefs<CR>
nmap <leader>tt :TernType<CR>

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

nmap <silent> t<C-n> :TestNearest<CR> " t Ctrl+n
nmap <silent> t<C-f> :TestFile<CR>    " t Ctrl+f
nmap <silent> t<C-s> :TestSuite<CR>   " t Ctrl+s
nmap <silent> t<C-l> :TestLast<CR>    " t Ctrl+l
nmap <silent> t<C-g> :TestVisit<CR>   " t Ctrl+gonsole.log(document.cookie);

nnoremap <C-n> :bnext<CR>
nnoremap <C-p> :bprevious<CR>

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})lug 'tpope/vim-surround'

" Map fzf commands
nmap <leader>k :Ag
nmap <leader>f :Files

map<C-s> :NERDTreeToggle<CR>

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

let g:GITLOG_default_mode = 2
map <silent> <f7> :call GITLOG_ToggleWindows()<cr>
map <silent> <f5> :call GITLOG_FlipWindows()<cr>

let g:ale_sign_error = '✗'
let g:ale_sign_warning = ''

let g:gitgutter_sign_added = ''
let g:gitgutter_sign_modified = ''
let g:gitgutter_sign_removed = ''
