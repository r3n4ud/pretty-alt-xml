require "lpeg"

local stack={}
local top={}
local P, S, V, C, R = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.R

local function xmldecl(txt)
   print("xmldecl",'#'..txt..'#')
end

local function comment(txt)
   print("comment",'#'..txt..'#')
end

local function doctype(txt)
   print("doctype",'#'..txt..'#')
end

local dquote=P('"')
local squote=P("'")
local equal=P("=")
--local space=P(" ")
local space=S(" \t\n\r")
local letter=R("az", "AZ")
local namestartchar=letter + '_' + ':'  -- See http://www.w3.org/TR/xml11/#NT-NameStartChar to complete
-- should include the following ranges: [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local namechar=namestartchar + R("09") + '-' + '.'
-- should include the following ranges: #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
local name=namestartchar * (namechar)^0
local versioninfo=P("version") * space^0 * equal * space^0 * ((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) -squote)^1 * squote)) 
local attribute=space^1 * name * space^0 * equal * space^0 * ((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) -squote)^1 * squote))


local comment_open=P("<!--")
local comment_close=P("-->")

-- '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
local doctype=(space^0 * P("<!DOCTYPE")) / doctype

local comment=(space^0 * comment_open * (P(1) - comment_close)^0 * comment_close) / comment

-- TODO? Make a specific pattern for SDdecl standalone=yes|no ?
local xmldecl=(space^0 * P("<?xml") * space^1 * versioninfo * attribute * space^0 * P("?>")) / xmldecl


local xml=P{
   [1]=xmldecl^0 * V(2),
   [2]=doctype^0 * comment^0,
}

local buffer =""
local count = 1
while true do
   local line = io.read()

   if line==nil then break
   else
      print(line)
      buffer = buffer..'\n'..line
   end
end

xml:match(buffer)

-- local lines = {}
-- -- read the lines in table 'lines'
-- for line in io.lines() do
--    table.insert(lines, line)
-- end
-- -- sort
-- table.sort(lines)
-- -- write all the lines
-- for i, l in ipairs(lines) do io.write(l, "\n") end
