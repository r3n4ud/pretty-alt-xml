-- Copyright 2010 Renaud Aubin <renaud.aubin@gmail.com>
-- Time-stamp: <2010-12-05 14:04:12>
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

if not modules then modules = { } end modules ['pret-xml'] = {
    version   = 0.1,
    comment   = "Time-stamp: <2010-11-20 19:31:14>",
    author    = "Renaud Aubin",
    copyright = "2010 Renaud Aubin",
    license   = "GNU General Public License version 3"
}

--[[
TODO:
− Manage NS with a dedicated color
− Parse inner CDATA data to colorize normal xml tag
− Improve space management within CharData
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
local utfcharacters, utfvalues, match, format = string.utfcharacters, string.utfvalues, string.match, string.format
local utfbyte, utffind = utf.byte, utf.find
local texwrite = tex.write
local color
local color_by_name = {}

local context = context -- ???
local obsspace = context.obs

local function flush_text(str)
   for c in utfcharacters(str) do
      if c == " " then obsspace() else texwrite(c) end
   end
end

local function texsprint(s)
   tex.sprint(tex.ctxcatcodes, s)
end

local function change_color(n)
   color = buffers.changestate(color_by_name[n], color)
end

local function finish_color()
   color = buffers.finishstate(color)
end


-- Needed by buffers.change_state:
for i, c in ipairs(colors) do
        color_by_name[c.name] = i
end
local function color_init()
        color = 0
        local def_colors = -- from pret-c.lua: \setupcolor[ema] introduces new line…
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

-- lpeg
local P, S, V, C, R = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.R

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
local debug=false

local function process(colorstr, txt)
   if(debug) then
      io.write("## "..colorstr..": "..txt.."#")
   end
   change_color(colorstr)
   flush_text(txt)
   finish_color()
end

local function prolog(txt)
   process("xmlprolog", txt)
end

local function Quoted(txt)
   process("quoted",txt)
end

local function Eq(txt)
   process("text",txt)
end

local function AttributeName(txt)
   process("attribute",txt)
end

local function Reference(txt)
   process("entity",txt)
end

local function Tag(txt)
   process("tag", txt)
end

local function Comment(txt)
   process("comment",txt)
end

local function CDSect(txt)
   process("cdata", txt)
end

local function CharData(txt)
   process("text", txt)
end

local function doctypedecl(txt)
   process("doctype",txt)
end

local function text(txt)
   process("text",txt)
end

local function MiscSpace(txt)
   process("text",txt)
end

local function xmlprolog(txt)
   process("xmlprolog",txt)
end

local function spacefunc(str)
   if(str=='\n') then
      if(debug) then
         io.write ("##EOL#\n")
      end
      context.doverbatimendofline()
   else
      flush_text(str)
   end
end

local function eolactiveonlyspacefunc(str)
   if(str=='\n') then
      if(debug) then
         io.write ("##EOL#\n")
      end
      context.doverbatimendofline()
   end
end

--------------------------------------------------------------------------------
-- Pattern definitions
--------------------------------------------------------------------------------
local dquote=P('"')
local squote=P("'")
local space=C(S(" \t\n\r")) / spacefunc 
local eolactiveonlyspace=C(S(" \t\n\r")) / eolactiveonlyspacefunc 
-- [25] Eq ::=  S? '=' S?
local Eq=(space^0 * P("=") * space^0) / text
local MiscSpace= C(space) / MiscSpace

local letter=R("az", "AZ")
local namestartchar=letter + '_' + ':'  -- See http://www.w3.org/TR/xml11/#NT-NameStartChar to complete
-- should include the following ranges: [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local namechar=namestartchar + R("09") + '-' + '.'
-- should include the following ranges: #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
local name=namestartchar * namechar^0

local attribute_lhs=name / AttributeName
local attribute_rhs=C((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) -squote)^1 * squote)) / Quoted
local attribute=space^1 * attribute_lhs * Eq * attribute_rhs
-- to delete???

-- [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
local Comment_open=C(eolactiveonlyspace^0 * P("<!--")) / Comment
local Comment_close=P("-->") / Comment
local Comment_content=( (eolactiveonlyspace + P(1) - P("-")) + (P("-") * (eolactiveonlyspace + P("1") - P("-")) ) ) / Comment
local Comment=space^0 * Comment_open * Comment_content^0 * Comment_close * space^0

--[16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
--[17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
local xmllabel=S("Xx")*S("Mm")*S("Ll")
local PITarget=C(name - xmllabel) / xmlprolog
local PI_open=P("<?") / xmlprolog
local PI_close=P("?>") / xmlprolog
local PI_content=(P(1) - P("?>"))^0 / xmlprolog
local PI= PI_open * PITarget * (MiscSpace * PI_content)^-1 * PI_close

-- '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
-- [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
-- [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
-- [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
-- [75] ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
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

-- -- [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
-- local AttInnerValue=S("<&")
-- local AttDQValue=(dquote * (Reference + (P(1) - AttInnerValue - dquote))^0 * dquote)
-- local AttSQValue=(squote * (Reference + (P(1) - AttInnerValue - squote))^0 * squote)

-- local AttValue=C(AttDQValue + AttSQValue) / Quoted
-- local AttName=C(name) / AttributeName

-- -- [41] Attribute ::= Name Eq AttValue
-- local Attribute=space^1 * AttName * Eq * AttValue
-- local Tag_attribute=Attribute * space^0

-- [42] ETag ::= '</' Name S? '>'
local ETag_open=C(P("</") * name) / Tag
local ETag_close=C(space^0 * P(">")) / Tag
local ETag=space^0 * ETag_open * ETag_close * space^0

-- [40] STag ::= '<' Name (S Attribute)* S? '>'  
local STag_close=C(P(">")) / Tag
local STag_open=C(space^0 * P("<") * name) / Tag
local STag=STag_open * attribute^0 * STag_close * space^0

-- [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
local EmptyElemTag_close=C(space^0 * P("/>")) / Tag
local EmptyElemTag_open=C(space^0 * P("<") * name) / Tag
-- local EmptyElemTag=EmptyElemTag_open * Tag_attribute^0 * EmptyElemTag_close
local EmptyElemTag=space^0 * EmptyElemTag_open * attribute^0 * EmptyElemTag_close * space^0 

-- [21] CDEnd ::= ']]>'
local CDEnd=P("]]>") / CDSect
-- [20] CData ::= (Char* - (Char* ']]>' Char*))
local CData=(eolactiveonlyspace + P(1) - P("]]>"))^0 / CDSect
-- [19] CDStart ::= '<![CDATA['
local CDStart=eolactiveonlyspace^0 * P("<![CDATA[") / CDSect
-- [18] CDSect ::=  CDStart CData CDEnd
local CDSect=CDStart * CData * CDEnd -- tested

-- [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
--local LegalCharData=eolactiveonlyspace^0 + P(1) - S("<&") / CharData
--local CharData=LegalCharData^0-- - (LegalCharData^0 * P("]]>") * LegalCharData^0 )
local CharData=(eolactiveonlyspace + P(1) - (S("<&") + P("]]>")))^0 / CharData

-- [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
-- [26] VersionNum ::='1.1' 
local Quoted=((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) - squote)^1 * squote)) / Quoted
local VersionName=P("version") / AttributeName
local VersionInfo=VersionName * Eq * Quoted

-- [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
local doctypedecl_open=C(P("<!DOCTYPE")) * eolactiveonlyspace / doctypedecl
local doctypedecl_content=name * (eolactiveonlyspace * ExternalID)^-1 * eolactiveonlyspace^-1 * (P("[") * (P(1) -P("]"))^1 * P("]") * eolactiveonlyspace^-1)^-1 / doctypedecl
local doctypedecl_close=P(">") / doctypedecl
local doctypedecl=doctypedecl_open * doctypedecl_content * doctypedecl_close * eolactiveonlyspace^0

-- [27] Misc ::= Comment | PI | S
local Misc=MiscSpace + Comment + PI

-- [23] XMLDecl ::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
-- TODO: Make specific patterns for EncodingDecl SDdecl
local XMLDecl_open=C(P("<?xml") * space^1) / xmlprolog
local XMLDecl_close=C(space^0 * P("?>")) / xmlprolog
local XMLDecl=XMLDecl_open * VersionInfo * attribute^0 *  XMLDecl_close * space^0 -- space^0 must be at the end of the line for spacefunc efficiency

-- [22] prolog ::= XMLDecl Misc* (doctypedecl Misc*)?
local prolog=XMLDecl * Misc^0 * (doctypedecl * Misc^0)^-1

 
local xmldoc=P {
   "document";
   -- [1] document ::= ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
   document=prolog * V("element") * Misc^0,
   -- [39] element ::= EmptyElemTag | STag content ETag
   element=EmptyElemTag + (STag * V("content") * ETag),
   -- [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
   content=(space + CharData)^-1 * ( (V("element") + Reference + CDSect + PI + Comment) * (space + CharData)^-1 )^0,
}

--------------------------------------------------------------------------------
-- Hooks (see http://wiki.contextgarden.net/Custom_pretty_printer and
-- buff-ini.lua).
--------------------------------------------------------------------------------
local first=false
local buffer=""

function visualizer.empty_line() 
   buffer = buffer.."\n"
end

function visualizer.end_of_line() end

function visualizer.begin_of_display()
   -- Initialize the buffer
   first=true
   buffer=""
   -- Initialize the color pallete
   color_init()
end

visualizer.begin_of_inline = visualizer.begin_of_display

function visualizer.end_of_display()
   finish_color()
   xmldoc:match(buffer)
end

visualizer.end_of_inline = visualizer.end_of_display

function visualizer.flush_line(str,nested)
   -- Populate the buffer
   if(first) then
      -- Don't add a newline for the first line
      first=false
      buffer=str
   else
      -- Concatenate a newline and the current line to the current buffer
      buffer = buffer.."\n"..str
   end
end
