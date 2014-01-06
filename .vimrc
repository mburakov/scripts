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

set nomousehide
set guioptions-=T
set guioptions-=m
set guifont=Liberation\ Mono\ 12

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

" Man function
function! s:OpenManPage()
  python import vim, subprocess, re
  python cword = vim.eval('expand ("<cword>")')
  python whatis = subprocess.Popen(['whatis', cword], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  python sect = whatis.wait() and [] or re.findall('\(([2,3,7])\)', whatis.stdout.read())
  python sect and (vim.command('silent !man {} {}'.format(sect[0], cword)), vim.command('redraw!'))
endfunction
com! Man call s:OpenManPage()

" Tweaks for console version
set t_Co=256

syntax on

imap [ []<LEFT>
imap ( ()<LEFT>
imap {<CR> {<CR>}<Esc>O

imap <TAB> <C-X><C-N>
imap <S-TAB> <C-V><TAB>

nmap <F2> :w<CR>
nmap <F7> :make<CR>:cw<CR>
nmap <F12> :DiffSVN<CR>
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

" tmux sends these for Ctrl+Arrow
map <ESC>[D <C-Left>
map! <ESC>[D <C-Left>
map <ESC>[B <C-Down>
map! <ESC>[B <C-Down>
map <ESC>[A <C-Up>
map! <ESC>[A <C-Up>
map <ESC>[C <C-Right>
map! <ESC>[C <C-Right>

colorscheme desertEx

" Airline
set laststatus=2
let g:airline_powerline_fonts=1

" C++ completion
let g:clang_complete_auto=1
let g:clang_auto_select=1
autocmd FileType c,cpp imap <buffer> <TAB> <C-X><C-U>
autocmd FileType c,cpp nmap <buffer> <F1> :Man<CR>

" General completion stuff
set completeopt=menuone,menu,longest

filetype on
filetype plugin on
filetype indent on
