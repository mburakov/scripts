set autochdir
set autoindent
set bs=2
set expandtab
set hidden
set hlsearch
set incsearch
set nobackup
set nocompatible
set noswapfile
set number
set shiftwidth=2
set smartindent
set tabstop=2

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

" Leader
let mapleader = ' '

imap [ []<LEFT>
imap ( ()<LEFT>
imap {<CR> {<CR>}<Esc>O
imap <TAB> <C-X><C-N>
nmap <F1> 3K
nmap <F2> :w<CR>
nmap <F7> :make<CR>:cw<CR>
nmap <F12> :DiffAny<CR>
nmap <TAB> :bn<CR>
nmap <S-TAB> :bp<CR>
nmap <C-C> :bd!<CR>:diffoff<CR>
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
Plug 'tpope/vim-fugitive'
Plug 'altercation/vim-colors-solarized'
Plug 'valloric/YouCompleteMe'
Plug 'critiqjo/lldb.nvim'
Plug 'vim-scripts/DoxygenToolkit.vim'
call plug#end()

" Solarized colorscheme
set background=dark
let g:solarized_termcolors=16
colorscheme solarized

" Powerline config
let g:airline_powerline_fonts=1

" YCM completion config
set completeopt-=preview
let g:ycm_confirm_extra_conf=0
nmap <C-]> :YcmCompleter GoTo<CR>

" PlantUML
autocmd BufRead,BufNewFile *.uml set filetype=uml
autocmd FileType uml set makeprg=plantuml\ -pipe\ <\ %\ \\\|\ feh\ -

" Use X clipboard as default register
set clipboard^=unnamed,unnamedplus

" Proper filename completion
set wildmode=list:longest
