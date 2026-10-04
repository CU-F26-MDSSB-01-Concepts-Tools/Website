-- {{< cite KEY >}} renders the full formatted bibliography entry for KEY
-- using the document's bibliography and CSL style (via pandoc citeproc).

local docMeta = nil

function Meta(meta)
  docMeta = meta
end

function cite(args)
  if docMeta == nil then
    return pandoc.Str("??cite: missing metadata??")
  end
  local key = pandoc.utils.stringify(args[1])

  -- Copy document metadata, then force a bibliography of just this key
  local m = {}
  for k, v in pairs(docMeta) do
    m[k] = v
  end
  m["nocite"] = pandoc.MetaInlines({ pandoc.Str("@" .. key) })
  m["suppress-bibliography"] = nil
  m["citeproc"] = nil

  local refsDiv = pandoc.Div({}, pandoc.Attr("refs", {}, {}))
  local mini = pandoc.Pandoc({ refsDiv }, m)
  local processed = pandoc.utils.citeproc(mini)

  local blocks = {}
  pandoc.walk(processed, {
    Div = function(d)
      if d.attr.identifier == "refs" then
        blocks = d.content
      end
      return nil
    end
  })

  if #blocks == 0 then
    return pandoc.Str("??cite: key '" .. key .. "' not found??")
  end
  return blocks
end
