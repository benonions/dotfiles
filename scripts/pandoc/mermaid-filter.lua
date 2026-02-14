local utils = (pandoc and pandoc.utils) or {}

local function sha1(s)
  if utils and utils.sha1 then
    return utils.sha1(s)
  end
  -- Fallback: simple non-cryptographic hash
  local h = 0
  for i = 1, #s do
    h = (h * 31 + s:byte(i)) % 4294967296
  end
  return string.format("%08x", h)
end

local function ensure_dir(path)
  local ok = os.execute(string.format("mkdir -p %q", path))
  return ok == true or ok == 0
end

local function render_mermaid(code)
  local tmpdir = os.getenv("MERMAID_TMPDIR") or "."
  ensure_dir(tmpdir)

  local hash = sha1(code)
  local mmd = tmpdir .. "/mermaid-" .. hash .. ".mmd"
  local out = tmpdir .. "/mermaid-" .. hash .. ".svg"

  local f = io.open(mmd, "w")
  if not f then
    return nil
  end
  f:write(code)
  f:close()

  local puppeteer_cfg = os.getenv("MERMAID_PUPPETEER_CONFIG")
  local cmd
  if puppeteer_cfg and puppeteer_cfg ~= "" then
    cmd = string.format("mmdc -p %q -i %q -o %q -b transparent", puppeteer_cfg, mmd, out)
  else
    cmd = string.format("mmdc -i %q -o %q -b transparent", mmd, out)
  end
  local ok = os.execute(cmd)
  if not (ok == true or ok == 0) then
    return nil
  end

  return pandoc.Para({ pandoc.Image("mermaid", out) })
end

function CodeBlock(el)
  local classes = el.classes or {}
  local is_mermaid = false
  for _, c in ipairs(classes) do
    if c == "mermaid" then
      is_mermaid = true
      break
    end
  end

  if not is_mermaid then
    return nil
  end

  return render_mermaid(el.text)
end

function RawBlock(el)
  if el.format ~= "html" then
    return nil
  end

  -- Handle <pre> wrapped fenced mermaid blocks.
  local content = el.text
  if not content:find("<pre>") then
    return nil
  end

  local code = content:match("```mermaid%s*(.-)```")
  if not code then
    return nil
  end

  return render_mermaid(code)
end
