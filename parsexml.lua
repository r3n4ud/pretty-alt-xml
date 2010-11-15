require "lpeg"

local stack={}
local top={}
local P, S, V, C, R = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.R

local function versioninfo(txt)
   print("versioninfo",'#'..txt..'#')
end

local function attribute(txt)
   print("attribute",'#'..txt..'#')
end

local function prolog(txt)
   print("prolog",'#'..txt..'#')
end

local function xmldecl_open(txt)
   print("xmldecl_open",'#'..txt..'#')
end

local function xmldecl_close(txt)
   print("xmldecl_close",'#'..txt..'#')
end


local function xmldecl(txt)
   print("xmldecl",'#'..txt..'#')
end

local function comment(txt)
   print("comment",'#'..txt..'#')
end

local function doctypedecl(txt)
   print("doctypedecl",'#'..txt..'#')
end

local function pi(txt)
   print("pi",'#'..txt..'#')
end

local dquote=P('"')
local squote=P("'")
local space=S(" \t\n\r")^1
local equal=space^0 * P("=") * space^0
--local space=P(" ")

local letter=R("az", "AZ")
local namestartchar=letter + '_' + ':'  -- See http://www.w3.org/TR/xml11/#NT-NameStartChar to complete
-- should include the following ranges: [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local namechar=namestartchar + R("09") + '-' + '.'
-- should include the following ranges: #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
local name=namestartchar * (namechar)^0
local versioninfo=P("version") * equal * ((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) -squote)^1 * squote)) / io.write
local attribute=space^1 * name * equal * ((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) -squote)^1 * squote)) / io.write

local comment_open=P("<!--")
local comment_close=P("-->")
local comment=(space^0 * comment_open * (P(1) - comment_close)^0 * comment_close * space^0) / io.write

--[16]          PI         ::=          '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
--[17]          PITarget           ::=           Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
local xmllabel=(S("Xx")*S("Mm")*S("Ll")) -- tested
local pitarget=(name - xmllabel) --tested
local pi=P("<?") * pitarget * (space * (P(1) - P("?>"))^0 )^-1 * P("?>") / io.write -- tested

local misc=(comment + pi + space)

-- '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
-- [11]         SystemLiteral      ::=          ('"' [^"]* '"') | ("'" [^']* "'")
-- [12]         PubidLiteral       ::=          '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
-- [13]         PubidChar          ::=          #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
-- [75]         ExternalID         ::=          'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
local pubidchar=S(" \n\r") + namechar + S("'()+,/=?;!*#@$%")
local pubidliteral=(dquote * pubidchar^0 * dquote) + (squote * (pubidchar - P("'"))^0 * squote)
local systemliteral=(dquote * (P(1)-dquote)^0 * dquote) + (squote * (P(1)-squote)^0 * squote)
local externalid=(P("SYSTEM") * space * systemliteral) + (P("PUBLIC") * space * pubidliteral * space * systemliteral)
-- local doctypedecl=P("<!DOCTYPE") * (space^1 * externalid)^-1 * space^-1 * (P("[") * (P(1) -P("]"))^1 * P("]") * space^-1)^-1 * P(">") / doctypedecl -- intSubset has been simplified

local doctypedecl=P("<!DOCTYPE") * space * name * (space * externalid)^-1 * space^-1 * (P("[") * (P(1) -P("]"))^1 * P("]") * space^-1)^-1 * P(">") / io.write -- intSubset has been simplified


-- TODO? Make a specific pattern for SDdecl standalone=yes|no ?
local xmldecl_open=space^0 * P("<?xml") * space^1 / io.write
local xmldecl_close=space^0 * P("?>") * space^0 / io.write
local xmldecl=(xmldecl_open * versioninfo * attribute *  xmldecl_close)

local prolog=xmldecl * misc^0 * (doctypedecl * misc^0)^-1


-- [1] document        ::=           ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
-- [39] element         ::=      EmptyElemTag | STag content ETag
-- [44] EmptyElemTag       ::=          '<' Name (S Attribute)* S? '/>'
-- [40]         STag       ::=          '<' Name (S Attribute)* S? '>'  
-- [41]         Attribute          ::=           Name Eq AttValue
-- [42]         ETag       ::=          '</' Name S? '>'
-- [43]         content    ::=           CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
-- [10]         AttValue           ::=          '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
local xml=P{
   [1]=prolog -- * V(2),
--   [2]=doctypedecl^0 * comment^0,
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
