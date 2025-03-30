require('lualine').setup {
  options = {
    theme = 'solarized',
  },
  sections = {
    lualine_b = {
      "branch",
      "diff",
      {
        "diagnostics",
        symbols = {
          error = _G['vscode-codicons'].error,
          warn = _G['vscode-codicons'].warning,
          info = _G['vscode-codicons'].info,
          hint = _G['vscode-codicons'].tasklist,
        },
      },
    },
    lualine_x = {
      "encoding",
      {
        "fileformat",
        symbols = {
          unix = '␊',
          dos = '␍␊',
          mac = '␍',
        },
      },
      "filetype",
    },
  },
}
