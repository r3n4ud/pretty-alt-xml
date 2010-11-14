if not modules then modules = { } end modules ['pret-xml'] = {
    version   = 1.001,
    comment   = "Time-stamp: <2010-11-14 18:17:07>",
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

-- function visualizer.reset()
--    state = 0
--    line_number = 0
--    startText = false
--    inText = false
--    stopText = false
--    startComment = false
--    inComment = false
--    stopComment = false
--    startDoctype = false
--    inDoctype = false
--    stopDoctype = false
--    startQuotedValue = false
--    inQuotedValue = false
--    stopQuotedValue = false
--    startSingleQuotedValue = false
--    inSingleQuotedValue = false
--    stopSingleQuotedValue = false
--    startEntity = false
--    inEntity = false
--    stopEntity = false
--    startTag = false
--    inTag = false
--    stopTag = false
--    startAttributeName = false
--    inAttributeName = true
--    stopAttributeName = false
--    startProcessingInstruction = false
--    inProcessingInstruction = false
--    stopProcessingInstruction = false
--    Equal = false
--    Invalid = false
--    startCDATA = false
--    inCDATA = false
--    stopCDATA = false
--    startXmlProlog = false
--    inXmlProlog = false
--    stopXmlProlog = false
-- end

local space = context.obs

local function flush_text(str)
   for c in utfcharacters(str) do
      if c == " " then space() else texwrite(c) end
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

function visualizer.begin_of_display()
   color_init()
end

visualizer.begin_of_inline = visualizer.begin_of_display
visualizer.end_of_display = finish_color
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
   recurse(str)
end
