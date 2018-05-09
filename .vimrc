" Indenting
set expandtab
set tabstop=2
set shiftwidth=2

" Highlight search hits
set hlsearch

" Turn on syntax highlightingg
syntax enable

"set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible

set wildmenu

set showcmd

set ruler

set confirm


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
call plug#end()

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

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})lug 'tpope/vim-surround'

" Map fzf commands
nmap <leader>k :Ag
nmap <leader>f :Files

" Activate Rainbow Parens
au VimEnter * RainbowParenthesesActivate
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces


