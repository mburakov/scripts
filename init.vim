"set autochdir
"set autoindent
"set bs=2
"set expandtab
"set hlsearch
"set incsearch
"set nobackup
set nocompatible
"set noswapfile
"set number
"set shiftwidth=2
"set smartindent
"set tabstop=2

"set t_Co=256

"filetype on
"filetype plugin indent on
"filetype indent on

call plug#begin('~/.config/nvim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'altercation/vim-colors-solarized'
call plug#end()

" Solarized colorscheme
set background=dark
let g:solarized_termcolors=16
colorscheme solarized

" Powerline config
let g:airline_powerline_fonts=1
