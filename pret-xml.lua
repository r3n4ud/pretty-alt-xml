if not modules then modules = { } end modules ['pret-xml'] = {
    version   = 1.001,
    comment   = "Time-stamp: <2010-11-20 00:20:13>",
    author    = "Renaud Aubin",
    copyright = "Renaud Aubin",
    license   = "see context related readme file"
}

--[[
TODO:
− Improve with http://lua-users.org/wiki/SwitchStatement
− Manage the syntax highlighting of tags like <svg:test> with a different color
− Parse inner CDATA data to colorize normal xml tag
− Simplify with functors
− Improve recurse_attribute (detect invalid, detect =)
− Use oxygen xml -like color scheme
− Clean up unused variables
--]]

local visualizer = buffers.newvisualizer("xml")

-- The colors are configurable by the user.
-- Example: \definecolor[XMLcomment][darkblue]
local colors = {
   {name = "text",                      color = "black"},
   {name = "comment",                   color = "darkgreen"},
   {name = "doctype",                   color = "blue"},
   {name = "quoted",                    color = "darkorange4"},
   {name = "entity",                    color = "yellow4"},
   {name = "tag",                       color = "darkblue"},
   {name = "attribute",                 color = "tan2"},
   {name = "equal",                     color = "black"},
   {name = "invalid",                   color = "red"},
   {name = "cdata",                     color = "green4"},
   {name = "xmlprolog",                 color = "blueviolet"}
}

local utf = unicode.utf8
local utfcharacters, utfvalues = string.utfcharacters, string.utfvalues
local utfbyte, utffind = utf.byte, utf.find
local texwrite = tex.write

--local buffers = buffers
local context = context

local color, state, line_number, startText, inText, stopText, startComment, inComment, stopComment, startDoctype, inDoctype, stopDoctype, startQuotedValue, inQuotedValue, stopQuotedValue, startSingleQuotedValue, inSingleQuotedValue, stopSingleQuotedValue, startEntity, inEntity, stopEntity, startTag, inTag, stopTag, startAttributeName, inAttributeName, stopAttributeName, startProcessingInstruction, inProcessingInstruction, stopProcessingInstruction, Equal, Invalid, startCDATA, inCDATA, stopCDATA, startXmlProlog, inXmlProlog, stopXmlProlog

local color_by_name = {}

local function texsprint(s)
   tex.sprint(tex.ctxcatcodes, s)
end

local function change_color(n)
   color = buffers.changestate(color_by_name[n], color)
end

local function finish_color()
   color = buffers.finishstate(color)
end

local match = string.match
local format = string.format
local obsspace = context.obs

local function flush_text(str)
   for c in utfcharacters(str) do
      if c == " " then obsspace() else texwrite(c) end
   end
end

-- Needed by buffers.change_state:
for i, c in ipairs(colors) do
        color_by_name[c.name] = i
end
local function color_init()
        color = 0
        local def_colors = -- from pret-c.lua: \setupcolor[ema] introduces new line...
      "\\definecolor [darkgreen]       [g=.392157]" ..
      "\\definecolor [darkorange4]     [r=.545098,g=.270588]" ..
      "\\definecolor [yellow4]         [r=.545098,g=.545098]" ..
      "\\definecolor [darkblue]        [b=.545098]" ..
      "\\definecolor [tan2]            [r=.933333,g=.603922,b=.286275]" ..
      "\\definecolor [green4]          [g=.545098]" ..
      "\\definecolor [blueviolet]      [r=.541176,g=.168627,b=.886275]"
   local palet = "\\definepalet[XMLcolorpretty]["
   for _, c in ipairs(colors) do
      def_colors = format(
         "%s\\doifcolorelse{XML%s}{}{\\definecolor[XML%s][%s]}",
         def_colors, c.name, c.name, c.color)
      palet = format("%s%s=XML%s,", palet, c.name, c.name)
   end
   palet = palet:gsub("(.+),$", "%1]")
   print("XXX", def_colors)
   print("XXX", palet)
   texsprint(def_colors)
   texsprint(palet)
   buffers.currentcolors = {}
   for i, c in ipairs(colors) do
      buffers.currentcolors[i] = c.name
   end
end

local buffer = ""

-- lpeg
local P, S, V, C, R = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.R

local function test(colorstr, txt)
   print("## "..colorstr..": "..txt.."#") 
   change_color(colorstr)
   flush_text(txt)
   finish_color()
end

local function prolog(txt)
   test("xmlprolog", txt)
end

local function Quoted(txt)
   test("quoted",txt)
end

local function Eq(txt)
   test("text",txt)
end

local function AttributeName(txt)
   test("attribute",txt)
end

local function Reference(txt)
   test("entity",txt)
end

local function Tag(txt)
   test("tag", txt)
end

local function Comment(txt)
   test("comment",txt)
end

local function CDSect(txt)
   test("cdata", txt)
end

local function CharData(txt)
   test("text", txt)
end

local function doctypedecl(txt)
   test("doctype",txt)
end

local function text(txt)
   test("text",txt)
end

local function xmlprolog(txt)
   test("xmlprolog",txt)
end

local function pipo(a,b,c)
   test("attribute",a)
   test("text",b)
   test("quoted",c)
end


local function spacefunc(str)
   print ("str.len = ", string.len(str))
   if(str=='\n') then
      print ("EOL detected ")
      context.doverbatimendofline()
   else
      flush_text(str)
   end
--   return str
end

local dquote=P('"')
local squote=P("'")
local space=C(S(" \t\n\r")) / spacefunc 
local equal=space^0 * P("=") * space^0

local letter=R("az", "AZ")
local namestartchar=letter + '_' + ':'  -- See http://www.w3.org/TR/xml11/#NT-NameStartChar to complete
-- should include the following ranges: [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local namechar=namestartchar + R("09") + '-' + '.'
-- should include the following ranges: #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
local name=namestartchar * namechar^0

local attribute=C(space^1 * name) * C(equal) * C((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) -squote)^1 * squote)) / pipo
-- to delete???

local Comment_open=P("<!--")
local Comment_close=P("-->") * space^0
local Comment=C(space^0 * Comment_open * (space + P(1) - Comment_close)^0 * Comment_close) / Comment

--[16]          PI         ::=          '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
--[17]          PITarget           ::=           Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
local xmllabel=S("Xx")*S("Mm")*S("Ll") -- tested
local PITarget=name - xmllabel --tested
local PI=P("<?") * PITarget * (space * (P(1) - P("?>"))^0 )^-1 * P("?>") -- tested

-- '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
-- [11]         SystemLiteral      ::=          ('"' [^"]* '"') | ("'" [^']* "'")
-- [12]         PubidLiteral       ::=          '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
-- [13]         PubidChar          ::=          #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
-- [75]         ExternalID         ::=          'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
local PubidChar=S(" \n\r") + namechar + S("'()+,/=?;!*#@$%")
local PubidLiteral=(dquote * PubidChar^0 * dquote) + (squote * (PubidChar - P("'"))^0 * squote)
local SystemLiteral=(dquote * (P(1)-dquote)^0 * dquote) + (squote * (P(1)-squote)^0 * squote)
local ExternalID=(P("SYSTEM") * space * SystemLiteral) + (P("PUBLIC") * space * PubidLiteral * space * SystemLiteral)

-- Custom HexChar pattern used by CharRef
local HexChar=R("09","af","AF")

-- [66] CharRef ::= ('&#' [0-9]+ ';') | ('&#x' [0-9a-fA-F]+ ';')
local CharRef= (P("&#") * R("09")^1 * P(";")) + (P("&#x") * HexChar^1 * P(";"))
-- [68] EntityRef    ::=  '&' Name ';'
local EntityRef=P("&") * name * P(";")
-- [69] PEReference  ::=  '%' Name ';'
local PEEntityRef=P("%") * name * P(";") -- not used!?

-- [67] Reference ::= EntityRef | CharRef
local Reference=C(EntityRef + CharRef) / Reference

-- [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
local AttInnerValue=S("<&")
local AttDQValue=(dquote * (Reference + (P(1) - AttInnerValue - dquote))^0 * dquote)
local AttSQValue=(squote * (Reference + (P(1) - AttInnerValue - squote))^0 * squote)

local AttValue=C(AttDQValue + AttSQValue) / Quoted
local AttName=C(name) / AttributeName

-- [41] Attribute ::= Name Eq AttValue
local Attribute=space^1 * AttName * Eq * AttValue

local Tag_attribute=Attribute * space^0 / io.write

-- [42] ETag ::= '</' Name S? '>'
local ETag=C(space^0 * P("</") * name * space^0 * P(">") * space^0) / Tag

-- [40] STag ::= '<' Name (S Attribute)* S? '>'  
local STag_close=space^0 * P(">") / Tag
local STag_open=P("<") * name / Tag
-- local STag=STag_open * Tag_attribute^0 * STag_close
local STag=STag_open * attribute^0 * STag_close * space^0

-- [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
local EmptyElemTag_close=C(space^0 * P("/>")) / Tag
local EmptyElemTag_open=C(space^0 * P("<") * name) / Tag
-- local EmptyElemTag=EmptyElemTag_open * Tag_attribute^0 * EmptyElemTag_close
local EmptyElemTag=EmptyElemTag_open * attribute^0 * EmptyElemTag_close * space^0 

-- [21] CDEnd ::= ']]>'
local CDEnd=P("]]>")
-- [20] CData ::= (Char* - (Char* ']]>' Char*))
local CData=(P(1)-P("]]>"))^0
-- [19] CDStart ::= '<![CDATA['
local CDStart=P("<![CDATA[")
-- [18] CDSect ::=  CDStart CData CDEnd
local CDSect=C(CDStart * CData * CDEnd) / CDSect -- tested

-- [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
local CharData=C((P(1)-S("<&"))^0 - ( -S("<&")^0 * P("]]>") * -S("<&")^0 )) / CharData

-- [25] Eq ::=  S? '=' S?
-- [26] VersionNum ::='1.1'
-- [27] Misc ::= Comment | PI | S




--------------------------------------------------------------------------------

local Quoted=(dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) - squote)^1 * squote)

local Eq=space^0 * P("=") * space^0

local VersionName=P("version")

-- [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
local VersionInfo=C(VersionName) * C(Eq) * C(Quoted) / pipo

--------------------------------------------------------------------------------
-- [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
local doctypedecl=C(P("<!DOCTYPE") * space * name * (space * ExternalID)^-1 * space^-1 * (P("[") * (P(1) -P("]"))^1 * P("]") * space^-1)^-1 * P(">") * space^0) / doctypedecl -- intSubset has been simplified

-- [27] Misc ::= Comment | PI | S
local MiscSpace = C(space) / text
local Misc=Comment + PI + MiscSpace

-- [23] XMLDecl ::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
-- TODO Make specific patterns for EncodingDecl SDdecl
local XMLDecl_open=C(space^0 * P("<?xml") * space^1) / xmlprolog
local XMLDecl_close=C(space^0 * P("?>")) / xmlprolog
local XMLDecl=XMLDecl_open * VersionInfo * attribute^0 *  XMLDecl_close * space^0 -- space^0 must be at the end of the line for spacefunc efficiency

--------------------------------------------------------------------------------
-- [22] prolog ::= XMLDecl Misc* (doctypedecl Misc*)?
local prolog=XMLDecl * Misc^0 * (doctypedecl * Misc^0)^-1

-- [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
-- [39] element ::= EmptyElemTag | STag content ETag
-- [1] document ::= ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
local xml=P {
   "document";
   document=prolog * V("element") * Misc^0,
   element=EmptyElemTag + (STag * V("content")), --* ETag),
   content= space^0 * CharData * space^0 * (V("element") + Comment)^0 * CharData,
--   content=CharData^-1 * ( (V("element") + Reference + CDSect + PI + Comment) * CharData^-1 )^0,
}







-- Hooks --
function visualizer.empty_line() end

function visualizer.end_of_line() end

function visualizer.begin_of_display()
   color_init()
end

visualizer.begin_of_inline = visualizer.begin_of_display

function visualizer.end_of_display()
   finish_color()
   xml:match(buffer)
end

visualizer.end_of_inline = visualizer.end_of_display

local before, capture, after
local attrbefore, attrcapture, attrafter

local function recurse_attribute(str)
   if (string.len(str)==0) then
      return
   elseif (startSingleQuotedValue or inSingleQuotedValue) then
      if (match(str,"(.-')(.*)")) then
         -- stopSingleQuotedValue
         attrcapture, str = match(str,"(.-')(.*)")
         change_color("quoted")
         flush_text(attrcapture)
         finish_color()
         startSingleQuotedValue=false
         inSingleQuotedValue=false
         recurse_attribute(str)
      else
         -- inSingleQuotedValue
         inSingleQuotedValue=true
         change_color("quoted")
         flush_text(str)
         finish_color()
      end
   elseif (startQuotedValue or inQuotedValue) then
      if (match(str,"(.-\")(.*)")) then
         -- stopQuotedValue
         attrcapture, str = match(str,"(.-\")(.*)")
         change_color("quoted")
         flush_text(attrcapture)
         finish_color()
         startQuotedValue=false
         inQuotedValue=false
         recurse_attribute(str)
      else
         -- inQuotedValue
         inQuotedValue=true
         change_color("quoted")
         flush_text(str)
         finish_color()
      end
   elseif (match(str,".-'.*")) then
      -- startSingleQuotedValue
      attrbefore, attrcapture, attrafter = match(str,"(.-)(')(.*)")
      recurse_attribute(attrbefore)
      change_color("quoted")
      flush_text(attrcapture)
      finish_color()
      startSingleQuotedValue = true
      recurse_attribute(attrafter)
   elseif (match(str,".-\".*")) then
      -- startQuotedValue
      attrbefore, attrcapture, attrafter = match(str,"(.-)(\")(.*)")
      recurse_attribute(attrbefore)
      change_color("quoted")
      flush_text(attrcapture)
      finish_color()
      startQuotedValue = true
      recurse_attribute(attrafter)
   else
      change_color("attribute")
      flush_text(str)
      finish_color()
      str=""
   end
end

local function recurse(str)
   if (string.len(str)==0) then
      return
   elseif (startTag or inTag) then
      if (match(str,".-/?>.*")) then
         -- stopTag
         before, capture, str = match(str,"(.-)(/?>)(.*)")
         recurse_attribute(before)
         change_color("tag")
         flush_text(capture)
         finish_color()
         startTag=false
         inTag=false
         recurse(str)
      else
         -- inTag
         inTag=true
         recurse_attribute(str)
      end
   elseif (startXmlProlog or inXmlProlog) then
      if (match(str,"(.-%?>)(.*)")) then
         -- stopXmlProlog
         capture, str = match(str,"(.-%?>)(.*)")
         change_color("xmlprolog")
         flush_text(capture)
         finish_color()
         startXmlProlog=false
         inXmlProlog=false
         recurse(str)
      else
         -- inXmlProlog
         inXmlProlog=true
         change_color("xmlprolog")
         flush_text(str)
         finish_color()
      end
   elseif (startCDATA or inCDATA) then
      if (match(str,"(.-%]%]>)(.*)")) then
         -- stopCDATA
         capture, str = match(str,"(.-%]%]>)(.*)")
         change_color("cdata")
         flush_text(capture)
         finish_color()
         startCDATA=false
         inCDATA=false
         recurse(str)
      else
         -- inCDATA
         inCDATA=true
         change_color("cdata")
         flush_text(str)
         finish_color()
      end
   elseif (startComment or inComment) then
      if (match(str,"(.-%-%->)(.*)")) then
         -- stopComment
         capture, str = match(str,"(.-%-%->)(.*)")
         change_color("comment")
         flush_text(capture)
         finish_color()
         startComment=false
         inComment=false
         recurse(str)
      else
         -- inComment
         inComment=true
         change_color("comment")
         flush_text(str)
         finish_color()
      end
   elseif (startDoctype or inDoctype) then
      if (match(str,"(.->)(.*)")) then
         -- stopDoctype
         capture, str = match(str,"(.->)(.*)")
         change_color("doctype")
         flush_text(capture)
         finish_color()
         startDoctype=false
         inDoctype=false
         recurse(str)
      else
         -- inDoctype
         inDoctype=true
         change_color("doctype")
         flush_text(str)
         finish_color()
      end
   elseif (match(str,"(.-)(<%a+%s-)(.*)")) then
      -- startTag
      before, capture, after = match(str,"(.-)(<%a+%s-)(.*)")
      recurse(before)
      startTag = true
      change_color("tag")
      flush_text(capture)
      finish_color()
      recurse(after)
   elseif (match(str,"(.-)(<%?)(.*)")) then
      -- startXmlProlog
      before, capture, after = match(str,"(.-)(<%?)(.*)")
      recurse(before)
      startXmlProlog = true
      change_color("xmlprolog")
      flush_text(capture)
      finish_color()
      recurse(after)
   elseif (match(str,"(.-)(<!%[)(.*)")) then
      -- startCDATA
      before, capture, after = match(str,"(.-)(<!%[)(.*)")
      recurse(before)
      startCDATA = true
      change_color("cdata")
      flush_text(capture)
      finish_color()
      recurse(after)
   elseif (match(str,"(.-)(<!%-%-)(.*)")) then
      -- startComment
      before, capture, after = match(str,"(.-)(<!%-%-)(.*)")
      recurse(before)
      startComment = true
      change_color("comment")
      flush_text(capture)
      finish_color()
      recurse(after)
   elseif (match(str,"(.-)(<!DOCTYPE)(.*)")) then
      -- startDoctype
      before, capture, after = match(str,"(.-)(<!DOCTYPE)(.*)")
      recurse(before)
      startDoctype = true
      change_color("doctype")
      flush_text(capture)
      finish_color()
      recurse(after)
   elseif (match(str,"(.-)(&%a+;)(.*)")) then
      -- Entity (atomic operation / i.e. no multiline treatment)
      before, capture, after = match(str,"(.-)(&%a+;)(.*)")
      recurse(before)
      change_color("entity")
      flush_text(capture)
      finish_color()
      recurse(after)
   elseif (match(str,"(.-)(</%a+>)(.*)")) then
      -- Closing Tag (atomic operation / i.e. no multiline treatment)
      before, capture, after = match(str,"(.-)(</%a+>)(.*)")
      recurse(before)
      change_color("tag")
      flush_text(capture)
      finish_color()
      recurse(after)
   else
      flush_text(str)
      str=""
   end
end

function visualizer.flush_line(str,nested)
--   recurse(str)
   print(str)
   buffer = buffer.."\n"..str
end


--- WARNING A NEW LINE IS ADDED SOMEHOW BY THE CURRENT LPEG PROCESSING
