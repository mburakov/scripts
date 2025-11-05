local function digest_row(line)
  local offset = line:find('|')
  if offset == nil then
    return nil
  end

  local prefix = line:sub(1, offset - 1)
  if line:find('^[^|]*|[%s|-]+|$') then
    return {
      prefix = prefix,
    }
  end

  local cells = {}
  line = line:sub(offset + 1)
  for column in line:gmatch('[^|]+') do
    table.insert(cells, column:match('^%s*(.-)%s*$'))
  end

  return {
    prefix = prefix,
    cells = cells,
  }
end

local function digest_current_table()
  local bufnr = vim.api.nvim_get_current_buf()
  local snapshot = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local current_line = vim.api.nvim_win_get_cursor(0)[1]

  local begin = current_line
  local sep = false
  local offset = 0
  local width = {}
  local data = {}

  local function do_line(line, reverse)
    local row = digest_row(snapshot[line])
    if not row then
      return false
    end

    sep = sep or (row.cells == nil)
    table.insert(data, reverse and 1 or #data + 1, {
      prefix = row.prefix,
      cells = row.cells,
    })

    offset = #row.prefix > offset and #row.prefix or offset
    for column, cell in ipairs(row.cells or {}) do
      width[column] = width[column] or 0
      width[column] = #cell > width[column] and #cell or width[column]
    end

    return true
  end

  for line = current_line, 1, -1 do
    if do_line(line, true) then
      begin = line
    else
      break
    end
  end

  for line = current_line + 1, #snapshot do
    if not do_line(line, false) then
      break
    end
  end

  return {
    begin = begin,
    sep = sep,
    offset = offset,
    width = width,
    data = data,
  }
end

local function reconstruct_current_table(tab)
  local bufnr = vim.api.nvim_get_current_buf()
  local lines = {}

  local function max(a, b)
    return a > b and a or b
  end

  for _, row in ipairs(tab.data) do
    local padding = string.rep(' ', tab.offset - #row.prefix)
    local result = row.prefix .. padding .. '|'
    for i, w in ipairs(tab.width) do
      w = tab.sep and max(w, 3) or w
      if not row.cells then
        result = result .. ' ' .. string.rep('-', w) .. ' |'
      else
        local cell = row.cells[i] or ''
        result = result .. ' ' .. cell ..
            string.rep(' ', w - #cell + 1) .. '|'
      end
    end
    table.insert(lines, result)
  end

  vim.api.nvim_buf_set_lines(bufnr,
    tab.begin - 1, tab.begin - 1 + #lines,
    false, lines)
end

local function reformat_current_table()
  local tab = digest_current_table()
  reconstruct_current_table(tab)
end

return {
  reformat_current_table = reformat_current_table,
}
