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
local space=S(" \t\n\r")
local equal=(space^0 * P("=") * space^0)
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


local HexChar=R("09","af","AF")
-- [66] CharRef ::= ('&#' [0-9]+ ';') | ('&#x' [0-9a-fA-F]+ ';')
local CharRef= (P("&#") * R("09")^1 * P(";")) + (P("&#x") * HexChar^1 * P(";"))
-- [68]  EntityRef    ::=  '&' Name ';'
local EntityRef=P("&") * name * P(";")
-- [69]  PEReference  ::=  '%' Name ';'
local PEEntityRef=P("%") * name * P(";")

-- [67]  Reference    ::=  EntityRef | CharRef
local Reference= EntityRef + CharRef

-- [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
local AttInnerValue=S("<&")
local AttDQValue=(dquote * (Reference + (P(1) - AttInnerValue - dquote))^0 * dquote) / io.write -- + (squote * (Reference + (P(1) - AttInnerValue - squote))^0 * squote) / io.write
local AttSQValue=(squote * (Reference + (P(1) - AttInnerValue - squote))^0 * squote) / io.write -- + (squote * (Reference + (P(1) - AttInnerValue - squote))^0 * squote) / io.write
local AttValue=AttDQValue + AttSQValue
-- Problème de capture à régler ↑↑↑↑↑↑

local Attribute=name * equal * AttValue / io.write
local Tag_attribute=Attribute * space^0 / io.write


local ETag=P("</") * name * space^-1 * P(">")
local STag_close=space * P(">") /io.write
local STag_open=P("<") * name / io.write
local STag=STag_open * Tag_attribute^0 * STag_close

local EmptyElemTag_close=space^0 * P("/>") /io.write
local EmptyElemTag_open=space^0 * P("<") * name / io.write
local EmptyElemTag=EmptyElemTag_open * Tag_attribute^0 * EmptyElemTag_close

-- CDATA Sections
-- [18]         CDSect     ::=           CDStart CData CDEnd
-- [19]         CDStart    ::=          '<![CDATA['
-- [20]         CData      ::=          (Char* - (Char* ']]>' Char*))
-- [21]         CDEnd      ::=          ']]>'
local CDEnd=P("]]>")
local CData=(P(1)-P("]]>"))^0
local CDStart=P("<![CDATA[")
local CDSect=CDStart * CData * CDEnd -- tested

-- Content of Elements
-- [43]  content      ::=  CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
-- [14]  CharData     ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
local CharData=(P(1)-S("<&"))^0 - ( -S("<&")^0 * P("]]>") * -S("<&")^0 )

-- local content=CharData^-1 * ( (element + Reference + CDSect + pi + comment) * CharData^-1 )^0 / io.write --------- work in progress

-- [39] element ::= EmptyElemTag | STag content ETag
-- local element=EmptyElemTag  + (STag * content * ETag)

-- [1] document        ::=           ( prolog element Misc* ) - ( Char* RestrictedChar Char* )

-- [44] EmptyElemTag       ::=          '<' Name (S Attribute)* S? '/>'
-- [40]         STag       ::=          '<' Name (S Attribute)* S? '>'  
-- [41]         Attribute          ::=           Name Eq AttValue
-- [42]         ETag       ::=          '</' Name S? '>'


local xml=P {
   "document";
   document=prolog, --* V("element") * misc^0,
   element=EmptyElemTag  + (STag * V("content") * ETag),
   content=CharData^-1 * ( (V("element") + Reference + CDSect + pi + comment) * CharData^-1 )^0,
}

print("\nprolog pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
local prolog_test=P { prolog, }
prolog_test:match([=[<?xml version="1.0" encoding="UTF-8"?>
<!-- That is a 


test -->
<!DOCTYPE mainTag SYSTEM "some.dtd" [ENTITY % entity]>
<!-- That is a test -->
<?oxygen RNGSchema="some.rng" type="xml"?><?xml version="1.0" encoding="UTF-8"?>
<!-- That is a 


test -->
<!DOCTYPE mainTag SYSTEM "some.dtd" [ENTITY % entity]>
<!-- That is a test -->
<?oxygen RNGSchema="some.rng" type="xml"?>]=])

print("\nEmptyElemTag pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
local EmptyElemTag_test=P { EmptyElemTag, }
EmptyElemTag_test:match("<root_node/>")

print("\nSTag_open pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
local STag_open_test=P { STag_open , }
assert(STag_open_test:match("<root_node test=\"eanrt\" >"), "check failed")

print("\nAttValue pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
local AttValue_test=P { AttValue , }
assert(AttValue_test:match("\"ea&lt;nrt\""), "check failed")

print("\nTag_attribute pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
local Tag_attribute_test=P { Tag_attribute , }
assert(Tag_attribute_test:match("test=\"eanrt\""), "check failed")






-- print("\ndocument pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")

-- local buffer=[=[<?xml version="1.0" encoding="UTF-8"?>
-- <!-- That is a 


-- test -->
-- <!DOCTYPE mainTag SYSTEM "some.dtd" [ENTITY % entity]>
-- <!-- That is a test -->
-- <?oxygen RNGSchema="some.rng" type="xml"?>
-- <mainTag>
--   <!-- This is a sample comment -->
--   <childTag attribute="Quoted Value">
--     <withTextContent>Some text content</withTextContent>
--     <withEntityContent>
--       Some text content with &lt;entities&gt;
--     </withEntityContent>
--     <otherTag attribute='Single quoted Value'invalid_text/>
--   </childTag>
--   <![CDATA[ some CData ]]>
-- </mainTag>
-- ]=]

-- xml:match(buffer)

print("")
print("−−−−−−−−−−−−−−− MATCH RESULT")


