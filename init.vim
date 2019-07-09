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

let g:LanguageClient_serverCommands = {
    \     'c': ['clangd'],
    \     'cpp': ['clangd'],
    \     'objcpp': ['clangd'],
    \ }
let g:LanguageClient_hoverPreview = 'Never'
let g:LanguageClient_diagnosticsSignsMax = 0
let g:airline_powerline_fonts = 1
let mapleader = ' '

colorscheme NeoSolarized

function CCompletionPrettifier(findstart, base) abort
    let l:result = LanguageClient_complete(a:findstart, a:base)
    if type(l:result) == type([])
        let l:result = luaeval('PrettifyCompletion(_A[1])', [l:result])
    endif
    return l:result
endfunction

function RestoreHighlight() abort
    hi comment gui=italic
    hi ALEError gui=undercurl guifg=red
    hi ALEWarning gui=undercurl guifg=orange
endfunction

function ToggleBackground() abort
    if &background == 'dark'
        set background=light
    else
        set background=dark
    endif
    call RestoreHighlight()
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
nmap <leader>f :call LanguageClient_textDocument_formatting()<CR>
nmap <leader>b :call ToggleBackground()<CR>
nmap <leader>d :lua MakeDoxygen()<CR>A
nmap K :vertical Man<CR>
tmap <ESC> <C-\><C-N>
vmap <leader>p :'<,'>w !plantuml -pipe \| feh -<CR><CR>

autocmd BufRead,BufNewFile *.uml set filetype=uml
autocmd FileType c setlocal completefunc=CCompletionPrettifier
autocmd FileType cpp setlocal completefunc=CCompletionPrettifier
autocmd FileType man wincmd L
autocmd FileType qf wincmd L
autocmd FileType tex set makeprg=pdflatex\ %
autocmd FileType uml set makeprg=plantuml\ -pipe\ <\ %\ \\\|\ feh\ -
autocmd QuitPre man://* :bd
autocmd VimEnter * call RestoreHighlight()

lua << EOF
function PrettifyCompletion(val)
    -- Potentially wrong glyphs:
    -- Function
    -- Constructor
    -- Text
    -- Module
    -- Unit
    -- Value
    local conversion = {
        Text = " ",
        Method = " ",
        Function = " ",
        Constructor = " ",
        Field = " ",
        Variable = " ",
        Class = " ",
        Interface = " ",
        Module = " ",
        Property = " ",
        Unit = " ",
        Value = " ",
        Enum = " ",
        Keyword = " ",
        Snippet = " ",
        Color = " ",
        File = " ",
        Reference = " ",
        Folder = " ",
        EnumMember = " ",
        Constant = " ",
        Struct = " ",
        Event = " ",
        Operator = " ",
        TypeParameter = " "
    }
    for k, v in ipairs(val) do
        local kind = conversion[v["kind"]]
        if kind == nil then
            kind = v["kind"]
        end
        v["abbr"] = kind .. " " .. v["abbr"]
        v["kind"] = nil
        if v["menu"] ~= nil and v["menu"] ~= "" then
            v["abbr"] = v["abbr"] .. " → " .. v["menu"]
            v["menu"] = nil
        end
    end
    return val
end

function MakeDoxygen()
    local lines = {}
    local linenr = vim.api.nvim_win_get_cursor(0)[1]
    local point = linenr - 1
    repeat
        local line = vim.api.nvim_buf_get_lines(0, linenr - 1, linenr, false)
        table.insert(lines, line[1])
        linenr = linenr + 1
    until (string.find(line[1], ")"))
    local comment = {"/**", " *"}
    local brackets = string.match(table.concat(lines), "%((.-)%)")
    for str in string.gmatch(brackets, "([^,]+)") do
        local name = string.match(str, "([^%s]+)%s*$")
        table.insert(comment, " * @param " .. name)
    end
    table.insert(comment, " * @return")
    table.insert(comment, " */")
    vim.api.nvim_buf_set_lines(0, point, point, false, comment)
    vim.api.nvim_win_set_cursor(0, {point + 2, 2})
end
EOF
