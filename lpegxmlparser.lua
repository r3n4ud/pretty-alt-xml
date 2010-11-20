require "lpeg"

local P, S, V, C, R = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.R

local dquote=P('"')
local squote=P("'")
local space=S(" \t\n\r")
local equal=C(space^0 * P("=") * space^0)
--local space=P(" ")

local letter=R("az", "AZ")
local namestartchar=letter + '_' + ':'  -- See http://www.w3.org/TR/xml11/#NT-NameStartChar to complete
-- should include the following ranges: [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local namechar=namestartchar + R("09") + '-' + '.'
-- should include the following ranges: #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
local name=C(namestartchar * namechar^0)

local attribute=C(space^1 * name * equal * ((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) -squote)^1 * squote))) / io.write
-- to delete???

local Comment_open=P("<!--")
local Comment_close=P("-->")
local Comment=(space^0 * Comment_open * (P(1) - Comment_close)^0 * Comment_close * space^0) / io.write

--[16]          PI         ::=          '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
--[17]          PITarget           ::=           Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
local xmllabel=(S("Xx")*S("Mm")*S("Ll")) -- tested
local PITarget=(name - xmllabel) --tested
local PI=P("<?") * PITarget * (space * (P(1) - P("?>"))^0 )^-1 * P("?>") / io.write -- tested


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
local Reference= EntityRef + CharRef

-- [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
local AttInnerValue=S("<&")
local AttDQValue=(dquote * (Reference + (P(1) - AttInnerValue - dquote))^0 * dquote) / io.write
local AttSQValue=(squote * (Reference + (P(1) - AttInnerValue - squote))^0 * squote) / io.write
local AttValue=AttDQValue + AttSQValue
-- Problème de capture à régler ↑↑↑↑↑↑

-- [41] Attribute ::= Name Eq AttValue
local Attribute=name * equal * AttValue / io.write
local Tag_attribute=Attribute * space^0 / io.write

-- [42] ETag ::= '</' Name S? '>'
local ETag=P("</") * name * space^-1 * P(">")

-- [40] STag ::= '<' Name (S Attribute)* S? '>'  
local STag_close=space * P(">")
local STag_open=P("<") * name
-- local STag=STag_open * Tag_attribute^0 * STag_close
local STag=C(STag_open * attribute^0 * STag_close)

-- [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
local EmptyElemTag_close=C(space^0 * P("/>"))-- /io.write
local EmptyElemTag_open=C(space^0 * P("<") * name)-- / io.write
-- local EmptyElemTag=EmptyElemTag_open * Tag_attribute^0 * EmptyElemTag_close
local EmptyElemTag=EmptyElemTag_open * attribute^0 * EmptyElemTag_close

-- [21] CDEnd ::= ']]>'
local CDEnd=P("]]>")
-- [20] CData ::= (Char* - (Char* ']]>' Char*))
local CData=(P(1)-P("]]>"))^0
-- [19] CDStart ::= '<![CDATA['
local CDStart=P("<![CDATA[")
-- [18] CDSect ::=  CDStart CData CDEnd
local CDSect=CDStart * CData * CDEnd -- tested

-- [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
local CharData=(P(1)-S("<&"))^0 - ( -S("<&")^0 * P("]]>") * -S("<&")^0 )

-- [25] Eq ::=  S? '=' S?
-- [26] VersionNum ::='1.1'
-- [27] Misc ::= Comment | PI | S


local elt_test= P {
   STag * CharData * ETag,
}

print("<test attr=\"bid\"> Some Text </test>")
print(elt_test:match("<test attr=\"bid\" > Some Text </test>"))








-- [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
local VersionInfo=P("version") * equal * ((dquote * (P(1) -dquote)^1 * dquote) + (squote * (P(1) - squote)^1 * squote)) / io.write

--------------------------------------------------------------------------------
-- [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
local doctypedecl=P("<!DOCTYPE") * space * name * (space * ExternalID)^-1 * space^-1 * (P("[") * (P(1) -P("]"))^1 * P("]") * space^-1)^-1 * P(">") / io.write -- intSubset has been simplified

-- [27] Misc ::= Comment | PI | S
local Misc=(Comment + PI)

-- [23] XMLDecl ::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
-- TODO Make specific patterns for EncodingDecl SDdecl
local XMLDecl_open=space^0 * P("<?xml") * space^1 / io.write
local XMLDecl_close=space^0 * P("?>") * space^0 / io.write
local XMLDecl=(XMLDecl_open * VersionInfo * attribute^0 *  XMLDecl_close)

--------------------------------------------------------------------------------
-- [22] prolog ::= XMLDecl Misc* (doctypedecl Misc*)?
local prolog=XMLDecl * Misc^0 * (doctypedecl * Misc^0)^-1

-- [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
-- [39] element ::= EmptyElemTag | STag content ETag
-- [1] document ::= ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
local xml=P {
   "document";
   document=prolog * V("element") * Misc^0,
   element=EmptyElemTag  + (STag * V("content") * ETag),
   content=CharData^-1 * ( (V("element") + Reference + CDSect + PI + Comment) * CharData^-1 )^0,
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
EmptyElemTag_test:match("<root_node etr=\"test\"/>")

print("\nSTag_open pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
local STag_open_test=P { STag_open , }
assert(STag_open_test:match("<root_node test=\"eanrt\" >"), "check failed")


print("\nAttValue pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
local AttValue_test=P { AttValue , }
assert(AttValue_test:match("\"ea&lt;nrt\""), "check failed")


-- print("\nTag_attribute pattern −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−")
-- local Tag_attribute_test=P { Tag_attribute , }
-- assert(Tag_attribute_test:match("test=\"eanrt\""), "check failed")









-- Unit Testing --

-- dquote
assert(dquote:match("\""), "check failed")
-- squote
assert(squote:match("'"), "check failed")
-- space
local space_test=P{ space^0, }
assert(space_test:match(" \n\t \t \n     \n\r"))
-- equal
assert(equal:match("   = \n"))
-- letter
local letter_test=P{ C(letter^0), }
assert(letter_test:match("thisisANSETe€st& 000 99   <\n")=="thisisANSETe")
-- namestartchar
-- namechar
-- name
local name_test=P{ C(name), }
assert(name_test:match("_This_is =\"a test\"")=="_This_is")
-- attribute
-- Comment_open
-- Comment_close
-- Comment
assert(Comment:match([=[<!-- That is a multiline


                           test --> auiet ainet ]=]))
-- xmllabel

-- PITarget
-- PI
-- PubidChar
-- PubidLiteral
-- SystemLiteral
-- ExternalID
-- HexChar
-- CharRef
-- EntityRef
-- PEEntityRef
-- Reference
-- AttInnerValue
-- AttDQValue
                           assert(AttDQValue:match("\"nte uant\""))

-- AttSQValue
-- AttValue
-- Attribute
-- Tag_attribute
-- ETag
-- STag_close
-- STag_open
-- STag
-- EmptyElemTag_close
-- EmptyElemTag_open
-- EmptyElemTag

-- CDEnd
-- CData
-- CDStart
-- CDSect
-- CharData
-- VersionInfo
-- doctypedecl
-- Misc
-- XMLDecl_open
-- XMLDecl_close
-- XMLDecl










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


