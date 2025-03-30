local function get_script_dir()
  local source = debug.getinfo(1, 'S').source:sub(2)
  return vim.fn.fnamemodify(source, ':p:h')
end

local function read_json(fname)
  local file = assert(io.open(fname))
  local data = file:read('*all')
  return vim.json.decode(data)
end

local function make_glyph(codepoint)
  return string.char(
    0xe0 + math.floor(codepoint / 0x1000),
    0x80 + math.floor(codepoint / 0x40) % 0x40,
    0x80 + codepoint % 0x40)
end

local fname = get_script_dir() .. '/mapping.json'
local data = read_json(fname)
local glyphs = {}

for k, v in pairs(data) do
  glyphs[k] = make_glyph(v) .. ' '
end

return glyphs
