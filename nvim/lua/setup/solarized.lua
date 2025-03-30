require('solarized').setup {
  on_highlights = function()
    local undercurl = {
      undercurl = true,
    }
    return {
      DiagnosticUnderlineError = undercurl,
      DiagnosticUnderlineWarn = undercurl,
      DiagnosticUnderlineInfo = undercurl,
      DiagnosticUnderlineHint = undercurl,
      DiagnosticUnderlineOk = undercurl,

      GitGutterAddLineNr = {
        bg = 'NvimLightGreen',
      },
      GitGutterChangeLineNr = {
        bg = 'NvimLightYellow',
      },
      GitGutterDeleteLineNr = {
        bg = 'NvimLightRed',
      },
    }
  end,
}
