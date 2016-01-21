set autochdir
set autoindent
set bs=2
set expandtab
set hlsearch
set incsearch
set nobackup
set nocompatible
set noswapfile
set number
set shiftwidth=2
set smartindent
set tabstop=2

"set t_Co=256

"filetype on
"filetype plugin indent on
"filetype indent on

" Leader
let mapleader = ' '

imap [ []<LEFT>
imap ( ()<LEFT>
imap {<CR> {<CR>}<Esc>O
imap <TAB> <C-X><C-N>
imap <ESC>[Z <C-V><TAB>
nmap <F1> :silent !man -S 3,2,7,1 <cword><CR>:redraw!<CR>
nmap <F2> :w<CR>
nmap <F7> :make<CR>:cw<CR>
nmap <F12> :DiffAny<CR>
nmap <TAB> :bn<CR>
nmap <S-TAB> :bp<CR>
nmap <C-C> :bd!<CR>:diffoff<CR>
nmap <C-S> :!srcsync<CR>
nmap <C-Left> <C-w>h
nmap <C-Down> <C-w>j
nmap <C-Up> <C-w>k
nmap <C-Right> <C-w>l
nmap [ :cp<CR>
nmap ] :cn<CR>
nmap <leader><leader> :noh<CR>
vmap // :s/^/\/\//<CR>:noh<CR>
vmap # :s/^/#/<CR>:noh<CR>

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

" General completion stuff
set completeopt=menuone,menu,longest

" PlantUML
autocmd BufRead,BufNewFile *.uml set filetype=uml
autocmd FileType uml set makeprg=plantuml\ -pipe\ <\ %\ \\\|\ feh\ -

" Use X clipboard as default register
set clipboard+=unnamedplus
