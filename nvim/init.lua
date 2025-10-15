vim.cmd [[
  call plug#begin()
  Plug 'airblade/vim-gitgutter'
  Plug 'maxmx03/solarized.nvim'
  Plug 'nvim-lualine/lualine.nvim'
  Plug 'nvim-treesitter/nvim-treesitter',
  \ { 'do': ':TSUpdate', 'branch': 'main' }
  Plug 'tpope/vim-fugitive'
  call plug#end()
]]

_G['tab-hack'] = require('tab-hack')
_G['vscode-codicons'] = require('vscode-codicons')
for _, v in ipairs(vim.g.plugs_order) do
  require('setup.' .. v:gsub('%..*$', ''))
end

vim.cmd.colorscheme('solarized')

vim.opt.autochdir = true
vim.opt.clipboard:append { 'unnamed', 'unnamedplus' }
vim.opt.colorcolumn = '+1'
vim.opt.expandtab = true
vim.opt.list = true
vim.opt.listchars = 'tab:␉ ,trail:␠'
vim.opt.number = true
vim.opt.shiftwidth = 0
vim.opt.tabstop = 2
vim.opt.textwidth = 80
vim.opt.wildmode = 'list:longest'

vim.cmd [[
  "vim.opt.diffopt throws an error...
  set diffopt+=vertical
]]

vim.g.mapleader = ' '
vim.keymap.set('i', '<s-tab>', '<c-v><tab>')
vim.keymap.set('i', '<tab>', '<c-x><c-o>')
vim.keymap.set('n', '<leader><backspace>', ':bp<bar>sp<bar>bn<bar>bd<cr>')
vim.keymap.set('n', '<leader><down>', '<c-w><down>')
vim.keymap.set('n', '<leader><leader>', ':noh<cr>')
vim.keymap.set('n', '<leader><left>', '<c-w><left>')
vim.keymap.set('n', '<leader><right>', '<c-w><right>')
vim.keymap.set('n', '<leader><tab>', ':tabnext<cr>')
vim.keymap.set('n', '<leader><up>', '<c-w><up>')
vim.keymap.set('n', '<leader>t',
  ':lua _G["tab-hack"].reformat_current_table()<cr>')
vim.keymap.set('n', '<s-tab>', ':bp<cr>')
vim.keymap.set('n', '<tab>', ':bn<cr>')
vim.keymap.set('t', '<esc>', '<c-\\><c-n>')
vim.keymap.set('v', '<leader>p',
  ":'<,'>w !plantuml -tsvg -pipe |" ..
  'swayimg --config=viewer.transparency=\\#00000000 -<cr>')

vim.lsp.config('*', {
  on_attach = function(client, bufnr)
    local mapping = {
      ['<leader>/'] = vim.lsp.buf.hover,
      ['<leader>f'] = vim.lsp.buf.format,
      ['<leader>.'] = vim.lsp.buf.definition,
    }
    for k, v in pairs(mapping) do
      vim.api.nvim_buf_set_keymap(bufnr, 'n', k, '', {
        callback = v,
        noremap = true,
      })
    end
    vim.lsp.completion.enable(true, client.id, bufnr, {
      convert = function(item)
        local symbol_kind = vim.lsp.protocol.CompletionItemKind[item.kind]
        local kebab = symbol_kind:gsub('%u', function(c)
          return '-' .. c:lower()
        end)
        local glyph = _G['vscode-codicons']['symbol' .. kebab]
        local detail = item.detail and (' →  ' .. item.detail) or ''
        return {
          abbr = glyph .. ' ' .. item.label .. detail,
          kind = '',
          menu = '',
        }
      end,
    })
  end,
})

vim.diagnostic.config {
  virtual_lines = {
    current_line = true,
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = _G['vscode-codicons'].error,
      [vim.diagnostic.severity.WARN] = _G['vscode-codicons'].warning,
      [vim.diagnostic.severity.INFO] = _G['vscode-codicons'].info,
      [vim.diagnostic.severity.HINT] = _G['vscode-codicons'].tasklist,
    },
  },
}

vim.lsp.enable {
  'luals',
  'clangd',
}
