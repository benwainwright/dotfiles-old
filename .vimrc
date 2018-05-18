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

set relativenumber " Turn on relative line numbering
" Install vim-plug if it is missing
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif



" Declare plugins
call plug#begin('~/.vim/plugged') 
Plug 'kien/rainbow_parentheses.vim'
Plug 'pangloss/vim-javascript'
Plug 'junegunn/fzf.vim'
Plug '/usr/local/opt/fzf'
Plug 'itchyny/lightline.vim'
Plug 'vim-syntastic/syntastic'
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
call plug#end()

let g:syntastic_javascript_checkers=['eslint']

" If the plugged directory hasn't been created, install all plugins
if empty(glob('~/.vim/plugged'))
  PlugInstall
endif

colorscheme CandyPaper


" Key mappings

" Map leader key to be the space bar
let mapleader = "\<Space>"

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

nnoremap <C-n> :bnext<CR>
nnoremap <C-p> :bprevious<CR>

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})lug 'tpope/vim-surround'

" Map fzf commands
nmap <leader>k :Ag
nmap <leader>f :Files

nmap<C-s> :NERDTreeToggle<CR>

" Activate Rainbow Parens
au VimEnter * RainbowParenthesesActivate
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces


