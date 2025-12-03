" Copyright (C) 2025 Mihai Gabriel Marin .vimrc

" Basic

"syntax on
set tabstop=4
set shiftwidth=4
set expandtab
set ai
"set number
set hlsearch
set ruler
set noswapfile
set autoindent
set smartindent
"set relativenumber
set autowrite
set ignorecase
set smartcase
set incsearch
set showmode
set encoding=utf-8
set background=dark

" highlight trailing spaces
" src: https://www.reddit.com/r/linux/comments/fx5e4v/comment/fmw4tn8/
highlight WhitespaceEOL ctermbg=darkred guibg=darkred
match WhitespaceEOL /\s\+$/

" When open a new file remember the cursor position of the last editing
if has("autocmd")
        " When editing a file, always jump to the last cursor position
        autocmd BufReadPost * if line("'\"") | exe "'\"" | endif
endif

" Disable matching parenthesis
let g:loaded_matchparen=1

" Remaps

" Leader key
" let mapleader = ","

" Reload config
nnoremap <leader>so :so ~/.vimrc<CR>

" Function keys
set pastetoggle=<F3>
