-- {{< cite KEY >}} renders the full formatted bibliography entry for KEY
-- using the document's bibliography and CSL style (via pandoc citeproc).

return {
  ["cite"] = function(args, kwargs, meta)
    local key = pandoc.utils.stringify(args[1])
    local dbg = {}
    for k, v in pairs(meta) do dbg[#dbg+1] = k end
    table.sort(dbg)
    io.stderr:write("META KEYS: " .. table.concat(dbg, ",") .. "\n")

    -- Copy document metadata, then force a bibliography of just this key
    local m = {}
    for k, v in pairs(meta) do
      m[k] = v
    end
    m["nocite"] = pandoc.MetaInlines({ pandoc.Str("@" .. key) })
    m["suppress-bibliography"] = nil
    m["citeproc"] = nil

    local refsDiv = pandoc.Div({}, pandoc.Attr("refs", {}, {}))
    local mini = pandoc.Pandoc({ refsDiv }, m)
    local processed = pandoc.utils.citeproc(mini)

    local blocks = {}
    for _, b in ipairs(processed.blocks) do
      if b.t == "Div" and b.attr.identifier == "refs" then
        blocks = b.content
      end
    end

    if #blocks == 0 then
      return pandoc.Str("??cite: key '" .. key .. "' not found??")
    end
    return blocks
  end
}
