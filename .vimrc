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

colorscheme desertEx

" YouCompleteMe
let g:ycm_confirm_extra_conf=0
let g:ycm_add_preview_to_completeopt=0

" Airline
set laststatus=2
let g:airline_powerline_fonts=1

" C++ completion
let g:clang_complete_auto=1
let g:clang_auto_select=1
autocmd FileType c,cpp imap <buffer> <TAB> <C-X><C-U>
autocmd FileType c,cpp nmap <buffer> <F1> :Man<CR>

" Perl completion
autocmd FileType perl imap <buffer> <TAB> <C-X><C-O>

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
