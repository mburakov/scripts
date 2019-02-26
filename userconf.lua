local window = require("window")

function dump(tbl, prefix, depth)
    local function quote(op)
        return "\"" .. tostring(op) .. "\""
    end
    local function strkey(op)
        return op and quote(op) .. ": " or ""
    end
    local ndepth = depth or 0
    local indent = string.rep("  ", ndepth);
    print(indent .. strkey(prefix) .. "{")
    for k, v in pairs(tbl) do
        if (type(v) == "table") then
            dump(v, k, ndepth + 1)
        else
            local kindent = indent .. "  "
            print(kindent .. strkey(k) .. quote(v))
        end
    end
    print(indent .. "}")
end

local realclose = window["methods"]["close_win"]
window["methods"]["close_win"] = function(w, b)
    msg.info("saving session")
    w:save_session()
    realclose(w, b)
end

window.add_signal("build", function(w)
    w.win.decorated = false
end)
