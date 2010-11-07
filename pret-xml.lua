if not modules then modules = { } end modules ['pret-xml'] = {
    version   = 1.001,
    comment   = "Time-stamp: <2010-11-08 00:10:12>",
    author    = "Renaud AUBIN",
    copyright = "Renaud AUBIN",
    license   = ""
}


local utf = unicode.utf8
local utfcharacters, utfvalues = string.utfcharacters, string.utfvalues
local utfbyte, utffind = utf.byte, utf.find
local texwrite = tex.write

local buffers = buffers
local context = context

local changestate, finishstate = buffers.changestate, buffers.finishstate

local visualizer = buffers.newvisualizer("xml")

local match = string.match

-- 0 0 0        --  1 Text
-- 0 100 0      --  2 Comment
-- 0 0 255      --  3 Doctype
-- 153 51 0     --  4 Quoted Value
-- 153 51 0     --  5 Single quoted Value
-- 150 150 0    --  6 Entity
-- 0 0 150      --  7 Tag
-- 245 132 76   --  8 Attribute name
-- 139 38 201   --  9 Processing Instruction
-- 255 128 64   -- 10 Equal
-- 255 0 0      -- 11 Invalid
-- 0 140 0      -- 12 CDATA
-- 139 38 201   -- 13 XML prolog

local colors = {
   "prettyone",
   "prettytwo",
   "prettythree",
   "prettyfour",
   "prettyone",
   "prettytwo",
   "prettythree",
   "prettyfour",
   "prettyone",
   "prettytwo",
   "prettythree",
   "prettyfour",
   "prettyone"
}

local states = {
   ["text"]=1,
   ["Comment"]=2,
   ["Doctype"]=3,
   ["Tag"]=7,
   ["CDATA"]=12,
   ["XmlProlog"]=13,
}

local state, line_number, startText, inText, stopText, startComment, inComment, stopComment, startDoctype, inDoctype, stopDoctype, startQuotedValue, inQuotedValue, stopQuotedValue, startSingleQuotedValue, inSingleQuotedValue, stopSingleQuotedValue, startEntity, inEntity, stopEntity, startTag, inTag, stopTag, startAttributeName, inAttributeName, stopAttributeName, startProcessingInstruction, inProcessingInstruction, stopProcessingInstruction, Equal, Invalid, startCDATA, inCDATA, stopCDATA, startXmlProlog, inXmlProlog, stopXmlProlog

function visualizer.reset()
   state = 0
   line_number = 0
   startText = false
   inText = false
   stopText = false
   startComment = false
   inComment = false
   stopComment = false
   startDoctype = false
   inDoctype = false
   stopDoctype = false
   startQuotedValue = false
   inQuotedValue = false
   stopQuotedValue = false
   startSingleQuotedValue = false
   inSingleQuotedValue = false
   stopSingleQuotedValue = false
   startEntity = false
   inEntity = false
   stopEntity = false
   startTag = false
   inTag = false
   stopTag = false
   startAttributeName = false
   inAttributeName = false
   stopAttributeName = false
   startProcessingInstruction = false
   inProcessingInstruction = false
   stopProcessingInstruction = false
   Equal = false
   Invalid = false
   startCDATA = false
   inCDATA = false
   stopCDATA = false
   startXmlProlog = false
   inXmlProlog = false
   stopXmlProlog = false
end

local space = context.obs

local function flush_text(str)
   for c in utfcharacters(str) do
      if c == " " then space() else texwrite(c) end
   end
end

local before, capture, after

local function recurse(str)
   if (string.len(str)==0) then
      return
   elseif (startXmlProlog or inXmlProlog) then
      if (match(str,"(.-%?>)(.*)")) then
         -- stopXmlProlog
         capture, str = match(str,"(.-%?>)(.*)")
         state = changestate(states["XmlProlog"], state)
         flush_text(capture)
         state = finishstate(state)
         startXmlProlog=false
         inXmlProlog=false
         recurse(str)
      else
         -- inXmlProlog
         inXmlProlog=true
         state = changestate(states["XmlProlog"], state)
         flush_text(str)
         state = finishstate(state)
      end
   elseif (startCDATA or inCDATA) then
      if (match(str,"(.-%]%]>)(.*)")) then
         -- stopCDATA
         capture, str = match(str,"(.-%]%]>)(.*)")
         state = changestate(states["CDATA"], state)
         flush_text(capture)
         state = finishstate(state)
         startCDATA=false
         inCDATA=false
         recurse(str)
      else
         -- inCDATA
         inCDATA=true
         state = changestate(states["CDATA"], state)
         flush_text(str)
         state = finishstate(state)
      end
   elseif (startComment or inComment) then
      if (match(str,"(.-%-%->)(.*)")) then
         -- stopComment
         capture, str = match(str,"(.-%-%->)(.*)")
         state = changestate(states["Comment"], state)
         flush_text(capture)
         state = finishstate(state)
         startComment=false
         inComment=false
         recurse(str)
      else
         -- inComment
         inComment=true
         state = changestate(states["Comment"], state)
         flush_text(str)
         state = finishstate(state)
      end
   elseif (startDoctype or inDoctype) then
      if (match(str,"(.->)(.*)")) then
         -- stopDoctype
         capture, str = match(str,"(.->)(.*)")
         state = changestate(states["Doctype"], state)
         flush_text(capture)
         state = finishstate(state)
         startDoctype=false
         inDoctype=false
         recurse(str)
      else
         -- inDoctype
         inDoctype=true
         state = changestate(states["Doctype"], state)
         flush_text(str)
         state = finishstate(state)
      end
   elseif (match(str,"(.-)(<%?)(.*)")) then
      -- startXmlProlog
      before, capture, after = match(str,"(.-)(<%?)(.*)")
      recurse(before)
      startXmlProlog = true
      state = changestate(states["XmlProlog"], state)
      flush_text(capture)
      state = finishstate(state)
      recurse(after)
   elseif (match(str,"(.-)(<!%[)(.*)")) then
      -- startCDATA
      before, capture, after = match(str,"(.-)(<!%[)(.*)")
      recurse(before)
      startCDATA = true
      state = changestate(states["CDATA"], state)
      flush_text(capture)
      state = finishstate(state)
      recurse(after)
   elseif (match(str,"(.-)(<!%-%-)(.*)")) then
      -- startComment
      before, capture, after = match(str,"(.-)(<!%-%-)(.*)")
      recurse(before)
      startComment = true
      state = changestate(states["Comment"], state)
      flush_text(capture)
      state = finishstate(state)
      recurse(after)
   elseif (match(str,"(.-)(<!DOCTYPE)(.*)")) then
      -- startDoctype
      before, capture, after = match(str,"(.-)(<!DOCTYPE)(.*)")
      recurse(before)
      startDoctype = true
      state = changestate(states["Doctype"], state)
      flush_text(capture)
      state = finishstate(state)
      recurse(after)
   else
      flush_text(str)
      str=""
   end
end

local line_number = 0

function visualizer.flush_line(str,nested)
   buffers.currentcolors = colors
   line_number = line_number + 1
   texio.write(line_number.." ")
   recurse(str)
   -- local startprolog = match(str,"(%s*<%?.-%?>%s*)")
   -- local doctype = match(str,"(%s*<!DOCTYPE.->%s*)") -- todo multiline
   -- local cdata   = match(str,"(%s*<!%[.-%]>%s*)") -- todo multiline
   -- local tag = match(str,"(%s*</?.->%s*)")

   -- if (prolog) then
   --    texio.write_nl("XML Prolog")
   --    state = changestate(states["XmlProlog"] ,state)
   --    flush_text(prolog)
   --    state = finishstate(state)
   -- elseif (doctype) then
   --    texio.write_nl("DOCTYPE")
   --    state = changestate(states["Doctype"] ,state)
   --    flush_text(doctype)
   --    state = finishstate(state)
   -- elseif (cdata) then
   --    texio.write_nl("CDATA")
   --    state = changestate(states["CDATA"] ,state)
   --    flush_text(cdata)
   --    state = finishstate(state)
   -- elseif (tag) then
   --    texio.write_nl("Tag")
   --    state = changestate(states["Tag"] ,state)
   --    flush_text(tag)
   --    state = finishstate(state)
   -- else
   --    texio.write_nl("NO PROCESSING")
   -- end
   texio.write_nl("")
end
