set autochdir
set background=light
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
Plug 'airblade/vim-gitgutter'
Plug 'maxmx03/solarized.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-orgmode/orgmode'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
call plug#end()

let g:airline_powerline_fonts = 1
let mapleader = ' '

colorscheme solarized
hi LspDiagnosticsUnderlineError gui=undercurl guisp=red
hi LspDiagnosticsUnderlineWarning gui=undercurl guisp=orange
hi Pmenu gui=NONE guibg=#eee8d5 guifg=#657b83
hi PmenuSel gui=NONE guibg=#fdf6e3 guifg=#657b83
hi comment gui=italic

imap ( ()<LEFT>
imap <S-TAB> <C-V><TAB>
imap <TAB> <C-X><C-O>
imap [ []<LEFT>
imap { {}<LEFT>
nmap <S-TAB> :bp<CR>
nmap <TAB> :bn<CR>
nmap <leader><BACKSPACE> :bp<bar>sp<bar>bn<bar>bd<CR>
nmap <leader><DOWN> <C-W><DOWN>
nmap <leader><LEFT> <C-W><LEFT>
nmap <leader><RIGHT> <C-W><RIGHT>
nmap <leader><TAB> :tabnext<CR>
nmap <leader><UP> <C-W><UP>
nmap <leader><leader> :noh<CR>
nmap <leader>d :lua make_doxygen()<CR>A
nmap <leader>t :tabe %<CR>
tmap <ESC> <C-\><C-N>
vmap <leader>p :'<,'>w !plantuml -pipe \| swayimg --config=viewer.transparency=\#00000000 -<CR>

autocmd BufRead,BufNewFile *.uml set filetype=uml
autocmd FileType man wincmd L
autocmd FileType tex set makeprg=lualatex\ -shell-escape\ %
autocmd FileType uml set makeprg=plantuml\ -pipe\ <\ %\ \\\|\ swayimg\ --config=viewer.transparency=\\#00000000\ -
autocmd FileType hare nmap <leader>/ :lua haredoc()<CR>
autocmd QuitPre man://* :bd

lua << EOF
require('nvim-treesitter.configs').setup {
  ensure_installed = 'all',
  highlight = {
    enable = true,
  },
}

require('orgmode').setup({
  org_agenda_files = '~/orgfiles/**/*'
})

local on_attach = function(client, bufnr)
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local opts = { noremap = true, silent = true }
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
  buf_set_keymap('n', '<leader>.', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
  buf_set_keymap('n', '<leader>/', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
  buf_set_keymap('n', '<leader>f', '<cmd>lua vim.lsp.buf.format()<cr>', opts)
end

local nvim_lsp = require('lspconfig')
local servers = {'clangd', 'lua_ls'}
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    signs = false,
  }
)

local vim_fn_complete = vim.fn.complete
vim.fn.complete = function(startcol, matches)
  local conversion = {
    Text = ' ',
    Method = ' ',
    Function = ' ',
    Constructor = ' ',
    Field = ' ',
    Variable = ' ',
    Class = ' ',
    Interface = ' ',
    Module = ' ',
    Property = ' ',
    Unit = ' ',
    Value = ' ',
    Enum = ' ',
    Keyword = ' ',
    Snippet = ' ',
    Color = ' ',
    File = ' ',
    Reference = ' ',
    Folder = ' ',
    EnumMember = ' ',
    Constant = ' ',
    Struct = ' ',
    Event = ' ',
    Operator = ' ',
    TypeParameter = ' '
  }
  for _, v in ipairs(matches) do
    local kind = conversion[v.kind]
    if kind == nil then
      kind = v.kind
    end
    v.kind = nil
    v.abbr = kind .. ' ' .. v.abbr
    if v.menu ~= nil and v.menu ~= '' then
      v.abbr = v.abbr .. ' →  ' .. v.menu
      v.menu = nil
    end
  end
  return vim_fn_complete(startcol, matches)
end

function make_doxygen()
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

local function charlen(s)
  local width = 0
  local esc = false
  s:gsub('.', function(c)
    local b = string.byte(c)
    if esc then
      if b == 0x6d then
        esc = false
      end
    else
      if b == 0x1b then
        esc = true
      else
        width = width + 1
      end
    end
  end)
  return width
end

function haredoc()
  local y, x = unpack(vim.api.nvim_win_get_cursor(0))
  local line = unpack(vim.api.nvim_buf_get_lines(0, y - 1, y, true))
  local head = line:sub(1, x):match('[%w:_]*$')
  local tail = line:sub(x + 1):match('^[%w:_]*')

  local lines = {}
  local max_width = 0
  local output = vim.fn.system({'haredoc', head .. tail})
  for line in output:gmatch('[^\n]+') do
    line = line:gsub('\t', '        ')
    table.insert(lines, ' ' .. line .. ' ')
    local width = charlen(lines[#lines])
    if width > max_width then
      max_width = width
    end
  end

  if next(lines) == nil then
    return
  end

  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, false, {
    relative = 'cursor',
    row = 0,
    col = 0,
    width = max_width,
    height = #lines,
    anchor = 'SW',
    style = 'minimal',
  })

  local chan = vim.api.nvim_open_term(buf, {})
  vim.api.nvim_chan_send(chan, table.concat(lines, '\r\n'))
  vim.api.nvim_create_autocmd({'CursorMoved'}, {
    once = true,
    callback = function(ev)
      vim.api.nvim_buf_delete(buf, {force = true})
    end,
  })
end
EOF
