set number
set tabstop=2
set shiftwidth=2
set expandtab
set smartindent
set autoindent
set bs=2
set nobackup
set noswapfile
set hidden
set incsearch
set hlsearch
set autochdir
set nowrap
set nocompatible

set nomousehide
set guioptions-=T
set guioptions-=m
set guifont=Liberation\ Mono\ for\ Powerline\ 24

" Use X clipboard as default register
set clipboard=unnamedplus

" Svn diff function
function! s:DiffWithSVNCheckedOut()
  let filetype = &ft
  diffthis
  vnew | r !svn cat #
  :1d
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSVN call s:DiffWithSVNCheckedOut()

" Git diff function
function! s:DiffWithGitHead()
  let filetype = &ft
  diffthis
  vnew | r !git show HEAD:./#
  :1d
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffGit call s:DiffWithGitHead()

" Route diff function
function! s:DiffWithAnything()
  call system("git status")
  if (!v:shell_error)
    call s:DiffWithGitHead()
    return
  endif
  call system("svn status")
  if (!v:shell_error)
    call s:DiffWithSVNCheckedOut()
    return
  endif
endfunction
com! DiffAny call s:DiffWithAnything()

" Tweaks for console version
set t_Co=256
let g:solarized_termcolors=16

" Leader
let mapleader = ' '

syntax on

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

" Neobundle
set runtimepath+=~/.vim/bundle/neobundle.vim/
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Rip-Rip/clang_complete'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'bling/vim-airline'
NeoBundle 'edkolev/tmuxline.vim'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'vim-scripts/DoxygenToolkit.vim'
call neobundle#end()
NeoBundleCheck

" clang-complete
let g:clang_complete_auto = 1
let g:clang_auto_select = 1
autocmd FileType c,cc,cpp imap <buffer> <TAB> <C-X><C-U>
autocmd FileType c,cc,cpp set tags=~/.vim/tags

" Airline
set laststatus=2
let g:airline_powerline_fonts = 1

" General completion stuff
set completeopt=menuone,menu,longest

filetype on
filetype plugin on
filetype indent on

" Pyclewn
nmap <F5> :C s<CR>
nmap <S-F5> :C fin<CR>
nmap <F6> :C n<CR>
nmap <F8> :C c<CR>
nmap <F9> :exe "C b " . expand("%:p") . ":" . line(".")<CR>
nmap <C-F9> :C d<CR>
nmap <C-PageUp> :C up<CR>
nmap <C-PageDown> :C down<CR>

" tmux compatibility for specific keys
map <ESC>[1;5D <C-Left>
map! <ESC>[1;5D <C-Left>
map <ESC>[1;5B <C-Down>
map! <ESC>[1;5B <C-Down>
map <ESC>[1;5A <C-Up>
map! <ESC>[1;5A <C-Up>
map <ESC>[1;5C <C-Right>
map! <ESC>[1;5C <C-Right>
map <ESC>[31~ <S-F5>
map! <ESC>[31~ <S-F5>
map <ESC>[20;5~ <C-F9>
map! <ESC>[20;5~ <C-F9>
map <ESC>[5;5~ <C-PageUp>
map! <ESC>[5;5~ <C-PageUp>
map <ESC>[6;5~ <C-PageDown>
map! <ESC>[6;5~ <C-PageDown>

" Set up colorscheme
set background=dark
colorscheme solarized
