" Gabriel Marin .vimrc
"
" Download Plug first:
" $ curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
" https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

" Plugins
call plug#begin()
  Plug 'tpope/vim-sensible'
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  Plug 'arzg/vim-colors-xcode'
call plug#end()

" Basic
syntax on
set tabstop=4
set shiftwidth=4
set expandtab
set ai
set number
set hlsearch
set ruler

" Color
colorscheme xcodedark 
hi Normal ctermbg=16 guibg=#000000
hi LineNr ctermbg=16 guibg=#000000
hi EndOfBuffer ctermbg=16 guibg=#000000

" Remaps
" Make SPACE leader key
nnoremap <SPACE> <Nop>
let mapleader = ' '
nnoremap <leader>pf :FZF<CR>
" nnoremap <leader>pf :call fzf#run({'sink': 'e'})<CR>

" Runtime path
set rtp+=$HOME/.vim/plugged/fzf/bin/fzf

