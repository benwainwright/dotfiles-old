syntax enable " Turn on syntax highlighting

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
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-surround'
Plug 'vim-syntastic/syntastic'
Plug 'airblade/vim-gitgutter'
call plug#end()

" If the plugged directory hasn't been created, install all plugins
if empty(glob('~/.vim/plugged'))
  PlugInstall
endif

" Activate Rainbow Parens
au VimEnter * RainbowParenthesesActivate
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
