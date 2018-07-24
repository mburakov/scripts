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
    \     'branch': 'next',
    \     'do': 'bash install.sh',
    \ }
Plug 'iCyMind/NeoSolarized'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
call plug#end()

let g:LanguageClient_diagnosticsDisplay = {
    \ 1: {
    \     'name': 'Error',
    \     'texthl': 'ALEError',
    \     'signText': '',
    \     'signTexthl': 'ALEErrorSign',
    \ },
    \ 2: {
    \     'name': 'Warning',
    \     'texthl': 'ALEWarning',
    \     'signText': '',
    \     'signTexthl': 'ALEWarningSign',
    \ },
    \ 3: {
    \     'name': 'Information',
    \     'texthl': 'ALEInfo',
    \     'signText': '',
    \     'signTexthl': 'ALEInfoSign',
    \ },
    \ 4: {
    \     'name': 'Hint',
    \     'texthl': 'ALEInfo',
    \     'signText': '',
    \     'signTexthl': 'ALEInfoSign',
    \ }}
let g:LanguageClient_serverCommands = {
    \     'cpp': ['clangd'],
    \     'c': ['clangd'],
    \ }
let g:LanguageClient_hoverPreview = 'Never'
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

function! PrettifySingle(key, val) abort
    if a:val['kind'] == 'Function'
        let a:val['kind'] = '  Function'
    elseif a:val['kind'] == 'Enum'
        let a:val['kind'] = '  Enum'
    elseif a:val['kind'] == 'Reference'
        let a:val['kind'] = '  Reference'
    elseif a:val['kind'] == 'Module'
        let a:val['kind'] = '  Module'
    elseif a:val['kind'] == 'Class'
        let a:val['kind'] = '  Class'
    elseif a:val['kind'] == 'Method'
        let a:val['kind'] = '  Method'
    elseif a:val['kind'] == 'Field'
        let a:val['kind'] = '﬍  Field'
    endif
    return a:val
endfunction

function! CCompletionPrettifier(findstart, base) abort
    let l:result = LanguageClient#complete(a:findstart, a:base)
    if type(l:result) == type([])
        let l:result = map(l:result, function('PrettifySingle'))
    endif
    return l:result
endfunction

imap ( ()<LEFT>
imap <S-TAB> <C-V><TAB>
imap <TAB> <C-X><C-U>
imap [ []<LEFT>
imap { {}<LEFT>
nmap <S-TAB> :bp<CR>
nmap <TAB> :bn<CR>
nmap <leader>. :call LanguageClient_textDocument_definition()<CR>
nmap <leader>/ :call LanguageClient_textDocument_hover()<CR>
nmap <leader><BACKSPACE> :bp<bar>sp<bar>bn<bar>bd<CR>
nmap <leader><DOWN> <C-W><DOWN>
nmap <leader><LEFT> <C-W><LEFT>
nmap <leader><RIGHT> <C-W><RIGHT>
nmap <leader><UP> <C-W><UP>
nmap <leader><leader> :noh<CR>
nmap K :vertical Man<CR>
tmap <ESC> <C-\><C-N>

autocmd BufRead,BufNewFile *.uml set filetype=uml
autocmd FileType c setlocal completefunc=CCompletionPrettifier
autocmd FileType cpp setlocal completefunc=CCompletionPrettifier
autocmd FileType man wincmd L
autocmd FileType qf wincmd L
autocmd FileType uml set makeprg=plantuml\ -pipe\ <\ %\ \\\|\ feh\ -
autocmd User LanguageClientStarted setlocal signcolumn=yes
autocmd User LanguageClientStopped setlocal signcolumn=auto
