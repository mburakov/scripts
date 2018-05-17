set autochdir
set background=dark
set clipboard+=unnamedplus
set colorcolumn=+1
set completeopt-=preview
set expandtab
set hidden
set inccommand=nosplit
set list
set listchars=tab:»\ ,trail:·
set number
set shiftwidth=2
set tabstop=2
set termguicolors
set textwidth=80
set wildmode=list:longest

call plug#begin('~/.local/share/nvim/plugged')
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'iCyMind/NeoSolarized'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
call plug#end()

let g:LanguageClient_serverCommands = {
    \ 'cpp': ['clangd'],
    \ 'c': ['clangd'],
    \ }
let g:LanguageClient_loadSettings = 1
let g:LanguageClient_settingsPath = '/home/mburakov/.config/nvim/settings.json'
let g:airline_powerline_fonts = 1
let mapleader = ' '

colorscheme NeoSolarized
hi comment gui=italic
hi ALEError gui=underline guifg=red
hi ALEWarning gui=underline guifg=orange

function ClangFormat()
  let where = line('.')
  :%!clang-format
  exe where
endfunction
command ClangFormat :call ClangFormat()

imap ( ()<LEFT>
imap <S-TAB> <C-V><TAB>
imap <TAB> <C-X><C-U>
imap [ []<LEFT>
imap { {}<LEFT>
nmap <S-TAB> :bp<CR>
nmap <TAB> :bn<CR>
nmap <leader>. :call LanguageClient_textDocument_definition()<CR>
nmap <leader>/ :call LanguageClient_textDocument_hover()<CR>
nmap <leader><BACKSPACE> :bd<CR>
nmap <leader><DOWN> <C-W><DOWN>
nmap <leader><LEFT> <C-W><LEFT>
nmap <leader><RIGHT> <C-W><RIGHT>
nmap <leader><UP> <C-W><UP>
nmap <leader><leader> :noh<CR>:sign unplace *<CR>
nmap K :vertical Man<CR>
tmap <ESC> <C-\><C-N>

autocmd BufRead,BufNewFile *.uml set filetype=uml
autocmd FileType c setlocal completefunc=LanguageClient#complete
autocmd FileType cpp setlocal completefunc=LanguageClient#complete
autocmd FileType man wincmd L
autocmd FileType qf wincmd L
autocmd FileType uml set makeprg=plantuml\ -pipe\ <\ %\ \\\|\ feh\ -
autocmd User LanguageClientStarted setlocal signcolumn=yes
autocmd User LanguageClientStopped setlocal signcolumn=auto

if exists('g:GtkGuiLoaded')
  call rpcnotify(1, 'Gui', 'Font', 'Iosevka medium 12')
endif
