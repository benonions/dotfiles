-- ~/.hammerspoon/init.lua
-- Bootloader: Hammerspoon loads Fennel, Fennel loads everything else.

-- Make ~/.hammerspoon a first-class Lua module path
package.path = package.path .. ";" .. hs.configdir .. "/?.lua"

-- Load vendored fennel runtime: ~/.hammerspoon/fennel.lua
_G.fennel = require("fennel")

-- Let fennel's require() find local .fnl modules in ~/.hammerspoon
fennel.path = fennel.path .. ";" .. hs.configdir .. "/?.fnl"
fennel["macro-path"] = fennel["macro-path"] .. ";" .. hs.configdir .. "/?.fnl"

-- Hook fennel into Lua's require() searchers
table.insert(package.searchers, 1, fennel.searcher)

require("hs.ipc") -- makes `hs` CLI available

hs.printf("Booting Fennel...")

local ok, err = pcall(function()
  -- Prefer dofile for "whole program" semantics
  fennel.dofile(hs.configdir .. "/main.fnl")
end)

if not ok then
  hs.alert.show("Fennel boot error (see console)", 2)
  hs.printf("BOOT ERROR:\n%s", err)
else
  hs.printf("Fennel system online")
end
