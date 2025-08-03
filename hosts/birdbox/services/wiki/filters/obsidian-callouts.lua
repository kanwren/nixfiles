local stringify = (require "pandoc.utils").stringify

function BlockQuote(el)
  local start = el.content[1]
  if (start.t == "Para" and start.content[1].t == "Str" and
        start.content[1].text:match("^%[!%w+%][-+]?$")) then
    local _, _, ctype = start.content[1].text:find("%[!(%w+)%]")
    el.content:remove(1)
    start.content:remove(1)
    local div = pandoc.Div(el.content, { class = "callout" })
    div.attributes["data-callout"] = ctype:lower()
    div.attributes["title"] = stringify(start.content):gsub("^ ", "")
    return div
  else
    return el
  end
end
