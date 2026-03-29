local function tmpfile(ext, data)
  local fname = vim.fn.tempname() .. '.' .. ext
  local f = assert(io.open(fname, 'w'))
  f:write('\xef\xbb\xbf')
  f:write(data)
  f:close()
  return fname
end

local function async(bufnr, ext, data)
  local fname = tmpfile(ext, data)
  vim.api.nvim_create_autocmd({ 'BufDelete' }, {
    buffer = bufnr,
    once = true,
    callback = function()
       os.remove(fname)
    end,
  })

  local browser = vim.env.BROWSER
  vim.system({ browser, fname })
end

local function bropen(command, ext)
  local bufnr = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  vim.system(command, { stdin = lines }, function(result)
    vim.schedule(function()
      async(bufnr, ext, result.stdout)
    end)
  end)
end

return bropen
