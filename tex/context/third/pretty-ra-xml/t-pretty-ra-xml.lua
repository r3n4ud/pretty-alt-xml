-- Copyright 2010 Renaud Aubin <renaud.aubin@gmail.com>
-- Time-stamp: <2010-12-19 02:24:08>
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
--
if not modules then modules = { } end modules ['t-pretty-ra-xml'] = {
   version   = 0.1,
   comment   = "Companion to t-pretty-ra-xml.mkiv",
   author    = "Renaud Aubin",
   copyright = "2010 Renaud Aubin",
   license   = "GNU General Public License version 3"
}

local P, S, V, C, R, patterns = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.R, lpeg.patterns

local context            = context
local verbatim           = context.verbatim
local makepattern        = visualizers.makepattern

local RaXmlSnippet         = context.RaXmlSnippet
local startRaXmlSnippet    = context.startRaXmlSnippet
local stopRaXmlSnippet     = context.stopRaXmlSnippet

local RaXmlSnippetProlog      = verbatim.RaXmlSnippetProlog
local RaXmlSnippetNSPref      = verbatim.RaXmlSnippetNSPref
local RaXmlSnippetQuoted      = verbatim.RaXmlSnippetQuoted
local RaXmlSnippetEq          = verbatim.RaXmlSnippetEq
local RaXmlSnippetAttName     = verbatim.RaXmlSnippetAttName
local RaXmlSnippetReference   = verbatim.RaXmlSnippetReference
local RaXmlSnippetTag         = verbatim.RaXmlSnippetTag
local RaXmlSnippetComment     = verbatim.RaXmlSnippetComment
local RaXmlSnippetCDSect      = verbatim.RaXmlSnippetCDSect
local RaXmlSnippetCharData    = verbatim.RaXmlSnippetCharData
local RaXmlSnippetDoctypeDecl = verbatim.RaXmlSnippetDoctypeDecl

local handler = visualizers.newhandler {
   startinline  = function() RaXmlSnippet(false,"{") end,
   stopinline   = function() context("}") end,
   startdisplay = function() startRaXmlSnippet() end,
   stopdisplay  = function() stopRaXmlSnippet () end,   
   prolog       = function(s) RaXmlSnippetProlog(s) end,
   nspref       = function(s) RaXmlSnippetNSPref(s) end,
   quoted       = function(s) RaXmlSnippetQuoted(s) end,
   eq           = function(s) RaXmlSnippetEq(s) end,
   attname      = function(s) RaXmlSnippetAttName(s) end,
   reference    = function(s) RaXmlSnippetReference(s) end,
   tag          = function(s) RaXmlSnippetTag(s) end,
   comment      = function(s) RaXmlSnippetComment(s) end,
   cdsect       = function(s) RaXmlSnippetCDSect(s) end,
   chardata     = function(s) RaXmlSnippetCharData(s) end,
   doctypedecl  = function(s) RaXmlSnippetDoctypeDecl(s) end,
}

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Pattern definitions
--------------------------------------------------------------------------------
local dquote=P('"')
local squote=P("'")
local letter=R("az", "AZ")
local namestartchar=letter + '_' --+ ':'  -- See http://www.w3.org/TR/xml11/#NT-NameStartChar to complete
-- should include the following ranges: [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local namechar=namestartchar + R("09") + '-' + '.'
-- should include the following ranges: #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
local name=namestartchar * namechar^0
local NCName=name - P(":")

-- [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
local Comment_open=P("<!--")
local Comment_close=P("-->")
local Comment_content=(P(1) - P("-")) + (P("-") * (P(1) - P("-")))

--[16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
--[17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
local xmllabel=S("Xx")*S("Mm")*S("Ll")
local PITarget=name - xmllabel -- Char is not properly defined yet
local PI_open=P("<?")
local PI_close=P("?>")
local PI_content=(P(1) - P("?>"))^0
--local PI= PI_open * PITarget * (MiscSpace * PI_content)^-1 * PI_close

-- '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
-- [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
-- [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
-- [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
-- [75] ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
local PubidChar=S(" \n\r") + namechar + S("'()+,/=?;!*#@$%")
local PubidLiteral=(dquote * PubidChar^0 * dquote) + (squote * (PubidChar - P("'"))^0 * squote)
local SystemLiteral=(dquote * (P(1)-dquote)^0 * dquote) + (squote * (P(1)-squote)^0 * squote)
local ExternalID=(P("SYSTEM") * patterns.whitespace * SystemLiteral) + (P("PUBLIC") * patterns.whitespace * PubidLiteral * patterns.whitespace * SystemLiteral)

-- Custom HexChar pattern used by CharRef
local HexChar=R("09","af","AF")

-- [66] CharRef ::= ('&#' [0-9]+ ';') | ('&#x' [0-9a-fA-F]+ ';')
local CharRef= (P("&#") * R("09")^1 * P(";")) + (P("&#x") * HexChar^1 * P(";"))
-- [68] EntityRef    ::=  '&' Name ';'
local EntityRef=P("&") * name * P(";")
-- [69] PEReference  ::=  '%' Name ';'
local PEReference=P("%") * name * P(";")

-- [67] Reference ::= EntityRef | CharRef
local Reference=EntityRef + CharRef

-- [21] CDEnd ::= ']]>'
local CDEnd=P("]]>")
-- [20] CData ::= (Char* - (Char* ']]>' Char*))
local CData=(patterns.whitespace + P(1) - P("]]>"))^0
-- [19] CDStart ::= '<![CDATA['
local CDStart=patterns.whitespace^0 * P("<![CDATA[")
-- [18] CDSect ::=  CDStart CData CDEnd
local CDSect=CDStart * CData * CDEnd -- tested

-- [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
-- [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
local EncodingDeclName=P("encoding")
local EncName=patterns.letter * (patterns.letter + patterns.digit + patterns.period + patterns.underscore + P("-") )^0 -- not used for the moment

local Quoted=((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) - squote)^1 * squote))
local VersionName=P("version")

-- [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
local doctypedecl_open=P("<!DOCTYPE")
-- local doctypedecl_content=name * (eolactiveonlyspace * ExternalID)^-1 * eolactiveonlyspace^-1 * (P("[") * (P(1) -P("]"))^1 * P("]") * eolactiveonlyspace^-1)^-1 / doctypedecl
local doctypedecl_close=P(">")
--local doctypedecl=doctypedecl_open * doctypedecl_content * doctypedecl_close -- * eolactiveonlyspace^0

-- [23] XMLDecl ::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
-- TODO: Make specific patterns for EncodingDecl SDdecl
local XMLDecl_open=P("<?xml")
local XMLDecl_close=P("?>")
--local XMLDecl=XMLDecl_open * VersionInfo * attribute^0 *  XMLDecl_close * space^0 -- space^0 must be at the end of the line for spacefunc efficiency

local grammar = visualizers.newgrammar(
   "default",
   { "visualizer",

     --[16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
     PI = makepattern(handler,"prolog",PI_open)
     * makepattern(handler,"prolog",PITarget)
     * (V("whitespace") * makepattern(handler,"prolog",PI_content))^-1
     * makepattern(handler,"prolog",PI_close),

     -- [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
     -- Fail to manage several empty lines
     Comment = makepattern(handler,"comment", Comment_open)
     * ( V("line") + V("whitespace")
     + makepattern(handler, "comment", Comment_content) )^0
     * makepattern(handler,"comment", Comment_close),

     -- [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
     -- EncName not implemented
     EncodingDecl = V("optionalwhitespace")
     * makepattern(handler,"attname",EncodingDeclName)
     * V("Eq")
     * makepattern(handler,"quoted",Quoted),

     -- [25] Eq ::=  S? '=' S?
     Eq = V("optionalwhitespace")
     * makepattern(handler,"eq",P("="))
     * V("optionalwhitespace"),

     -- [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
     -- [26] VersionNum ::='1.1' -- not implemented
     VersionInfo = V("whitespace")
     * makepattern(handler,"attname",VersionName)
     * V("Eq")
     * makepattern(handler,"quoted",Quoted),
  
     SQAttValue_content = makepattern(handler,"quoted",(P(1)-squote-P("<")-P("&")))
     + V("Reference"),

     DQAttValue_content = makepattern(handler,"quoted",(P(1)-dquote-P("<")-P("&")))
     + V("Reference"),

     -- [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
     AttValue = (makepattern(handler, "quoted", dquote)
     * V("DQAttValue_content")^0 * makepattern(handler, "quoted", dquote))
     + (makepattern(handler, "quoted", squote) 
     * V("SQAttValue_content")^0 * makepattern(handler, "quoted", squote)),

     -- [41] Attribute ::= Name Eq AttValue
     Attribute = makepattern(handler, "nspref", NCName * P(":"))^-1
     * (makepattern(handler,"nspref", P("xmlns")) + makepattern(handler,"attname", NCName))
     * V("Eq")
     * V("AttValue"),

     -- [40] STag ::= '<' Name (S Attribute)* S? '>'  
     STag = makepattern(handler, "tag", P("<"))
     * makepattern(handler, "nspref", NCName * P(":"))^-1
     * makepattern(handler, "tag", NCName)
     * (V("whitespace") * V("Attribute"))^0 * V("optionalwhitespace")
     * makepattern(handler, "tag", P(">")),

     -- [42] ETag ::= '</' Name S? '>'
     ETag = makepattern(handler, "tag", P("</"))
     * makepattern(handler, "nspref", NCName * P(":"))^-1
     * makepattern(handler, "tag", NCName)
     * V("optionalwhitespace") * makepattern(handler, "tag", P(">")),

     -- [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
     EmptyElemTag = makepattern(handler, "tag", P("<"))
     * makepattern(handler, "nspref", NCName * P(":"))^-1
     * makepattern(handler, "tag", NCName)
     * (V("whitespace") * V("Attribute"))^0 * V("optionalwhitespace")
     * makepattern(handler, "tag", P("/>")),

     CDSect = makepattern(handler,"cdsect",CDSect),

     CharData=(V("whitespace") + makepattern(handler,"default",P(1)) - (S("<&") + P("]]>")))^0,

     Reference = makepattern(handler,"reference",Reference),

     -- [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
     content = V("CharData")^-1 * (
      (V("Reference") + V("element") + V("PI") + V("Comment") + V("CDSect"))
      * (V("whitespace")+V("CharData"))^-1
     )^0,

     ExternalID=(makepattern(handler,"doctypedecl",P("SYSTEM"))
     * V("whitespace") * makepattern(handler,"doctypedecl",SystemLiteral))
     + (
      makepattern(handler,"doctypedecl",P("PUBLIC"))
      * V("whitespace") *
      makepattern(handler,"doctypedecl",PubidLiteral) 
      * V("whitespace")
      * makepattern(handler,"doctypedecl",SystemLiteral)
     ),

     -- [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
     doctypedecl = makepattern(handler,"doctypedecl",doctypedecl_open)
     * V("whitespace") * makepattern(handler,"doctypedecl", name)
     * (V("whitespace") * V("ExternalID"))^-1
     * V("optionalwhitespace")
     * (
      makepattern(handler,"doctypedecl",P("[") * (P(1) -P("]"))^1
      * P("]")) * V("optionalwhitespace")
     )^-1 -- to be replaced by intSubset
     * makepattern(handler,"doctypedecl",doctypedecl_close),

     -- [23] XMLDecl ::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
     XMLDecl = makepattern(handler,"prolog",XMLDecl_open)
     * V("VersionInfo")
     * V("EncodingDecl")^-1
     * V("optionalwhitespace")
     * makepattern(handler,"prolog",XMLDecl_close),

     -- [27] Misc ::= Comment | PI | S
     Misc = V("Comment") + V("whitespace") + V("PI"),

     -- [39] element ::= EmptyElemTag | STag content ETag       
     element = V("EmptyElemTag")
     + (V("STag") * V("content") * V("ETag")),

     -- [22] prolog ::= XMLDecl Misc* (doctypedecl Misc*)?
     prolog =  V("XMLDecl") * V("Misc")^0
     * (V("doctypedecl") * V("Misc")^0)^-1,

     -- [1] document ::= ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
     document=V("prolog") * V("element") * V("Misc")^0,

     pattern = V("document") + V("default"),
     visualizer = V("pattern")^1 

  }
)

local parser = P(grammar)

visualizers.register("raxml", { parser = parser, handler = handler, grammar = grammar } )
