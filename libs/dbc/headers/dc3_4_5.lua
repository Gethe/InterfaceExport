-- SPDX-FileCopyrightText: © 2024 foxlit <https://www.townlong-yak.com/dbc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = { fourCC = { WDC3=1, WDC4=1, WDC5=1 } }, require("dbc.bin")
local uint32_le, uint16_le, int_le = bin.uint32_le, bin.uint16_le, bin.int_le

local function assertLEQ(a, b, message)
	if a > b then
		error(message .. ": " .. a .. " > " .. b, 2)
	end
end
local function setFlags(f, ...)
	for i=1,select("#", ...) do
		local v = select(i, ...)
		if v then
			f[v] = true
		end
	end
end
local function serializeFlags(f)
	local fa = {}
	for k, v in pairs(f) do
		if v then
			fa[#fa+1] = k
		end
	end
	table.sort(fa)
	return table.concat(fa, " ")
end

function M:parseHeader(data, dbcM)
	local fourCC = data:sub(1,4)
	local is4, is5 = fourCC == 'WDC4', fourCC == 'WDC5'
	local h, feat, h0 = {parts={}}, {}, 0
	if is5 then
		local hv = uint32_le(data, 4)
		h0 = hv == 5 and 132 or error('DC5 unexpected header version [' .. hv .. ']')
	end
	h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, h0+4), uint32_le(data, h0+8), uint32_le(data, h0+12), uint32_le(data, h0+16)
	h.minId, h.maxId, h.locale = uint32_le(data, h0+28), uint32_le(data, h0+32), uint32_le(data, h0+36)
	h.flags = uint16_le(data, h0+40)
	h.fields2, h.packStart, h.lookupCount = uint32_le(data, h0+44), uint32_le(data, h0+48), uint32_le(data, h0+52)
	local keyColumnID = 1+uint16_le(data, h0+42)
	local packLength = uint32_le(data, h0+56)
	local codLength = uint32_le(data, h0+60)
	local palLength = uint32_le(data, h0+64)
	local numParts = uint32_le(data, h0+68)
	local fieldInfoPos = h0+ 72 + numParts*40
	local packPos = packLength > 0 and (fieldInfoPos + 4*h.fields)
	assert(h.fields == h.fields2, "DC3+ header fields/fields2 values are inconsistent")
	assert(packLength == 0 or packLength/24 == h.fields, "DC3+ packing parity check")
	local eidLength, eidPos = 0, (packPos or fieldInfoPos) + packLength + palLength + codLength
	
	local hasExternalPrimaryID, hasStringBlock, hasInlineStrings, hasForeignKey = false, false, false, false
	local partMeta = {__index=h}
	for i=1, numParts do
		local p = h0+32+i*40
		local pt = {
			partIndex=i,
			keyName = ("%02x%02x%02x%02x%02x%02x%02x%02x"):format(data:byte(p+1, p+8)),
			rowBase = uint32_le(data, p+8),
			rows = uint32_le(data, p+12),
			stringLength = uint32_le(data, p+16),
			rowsEnd = uint32_le(data, p+20),
			idmLength = uint32_le(data, p+24),
			relLength = uint32_le(data, p+28),
			rowListEntries = uint32_le(data, p+32),
			cloneLength = uint32_le(data, p+36)*8,
		}
		if pt.rowsEnd > 0 then
			p = pt.rowsEnd
			pt.rowsLength = p - pt.rowBase
		else
			pt.rowsLength = h.stride * pt.rows
			p = pt.rowBase + pt.rowsLength + pt.stringLength
			pt.stringBase = p - pt.stringLength + 1
			pt.rowRelativeStrings = true
			hasStringBlock = hasStringBlock or (pt.stringLength > 0)
		end
		if pt.idmLength > 0 then
			pt.idmOffset, hasExternalPrimaryID, p = p, true, p + pt.idmLength
		end
		if pt.cloneLength > 0 then
			assert(pt.cloneLength % 8 == 0, 'DC3+ row cloning length parity check')
			pt.cloneOffset, p = p, p + pt.cloneLength
		end
		if pt.rowListEntries > 0 then
			pt.rowListOffset, hasExternalPrimaryID, p = p, true, p + pt.rowListEntries*6
		end
		if pt.relLength > 0 then
			pt.relBase, p = p, p + pt.relLength
		end
		if pt.rowListEntries > 0 then
			pt.rowIDListOffset, p = p, p + pt.rowListEntries*4
		end
		pt.partEndOffset = p
		pt.isPresent = (data:match("()%Z", pt.rowBase) or pt.partEndOffset) < pt.partEndOffset
		assertLEQ(pt.partEndOffset, #data, "DC3+ data too short")
		
		if pt.isPresent then
			if pt.rowListOffset and pt.rows > 0 then
				local lp, ip = pt.rowListOffset, pt.rowIDListOffset
				local ot, minLen, maxLen, sz = {}, math.huge, -math.huge
				for i=1, pt.rowListEntries do
					lp, sz = lp + 6, uint16_le(data, lp+4)
					if sz <= 0 then error('DC3+ unexpected zero-length row') end
					ot[i], ip = {uint32_le(data, ip), uint32_le(data, lp-6), sz}, ip + 4
					minLen, maxLen = minLen < sz and minLen or sz, maxLen > sz and maxLen or sz
				end
				-- When all inline strings in the part are of the same length, this doesn't work.
				-- There's normally enough variation /somewhere/, so this sets the flag globally
				-- via the h.inlineStrings __index if any part has length variability.
				pt.rows, pt.rowList, pt.maxRowSize, pt.minRowSize, pt.inlineStrings = #ot, ot, maxLen, minLen, minLen ~= maxLen or nil
				hasInlineStrings = hasInlineStrings or pt.inlineStrings
				
				if dbcM.PREFER_ERRORS and pt.idmLength > 0 then
					local idmd = data:sub(pt.idmOffset+1, pt.idmOffset+pt.idmLength)
					local rlid = data:sub(pt.rowIDListOffset+1, pt.rowIDListOffset+4*pt.rowListEntries)
					assert(idmd == rlid, "DC3+ primary key disagreement")
				else
					feat.IgnoredIDMaps = feat.IgnoredIDMaps or pt.idmLength > 0
				end
			elseif pt.idmLength > 0 then
				local idMap, p = {}, pt.idmOffset
				for i=1, pt.rows do
					idMap[i], p = uint32_le(data, p), p + 4
				end
				pt.idMap = idMap
			end
			if pt.relLength > 0 then
				local p = pt.relBase
				local count, min, max = uint32_le(data, p), uint32_le(data, p+4), uint32_le(data, p+8)
				pt.fkField, hasForeignKey = {0,0, packType="ForeignKeyMap", adOfs=p+12, adLength=pt.relLength-12, pa1=count, pa2=min, pa3=max}, true
			end
		else
			feat.MissingParts = true
		end
		
		if (is4 or is5) and pt.keyName ~= "0000000000000000" then
			local eidCount = uint32_le(data, eidPos)
			local eidPartLength = 4 + eidCount * 4
			if eidCount < pt.rows and dbcM.PREFER_ERRORS then
				error("DC4+ encrypted ID list underflow (part header declares more rows)")
			end
			eidLength, eidPos = eidLength + eidPartLength, eidPos + eidPartLength
		end
		
		h.parts[i] = setmetatable(pt, partMeta)
	end
	for i=1, numParts do
		assertLEQ(eidPos, h.parts[i].rowBase, "DC3+ part data in header region")
	end
	if hasStringBlock then -- addressing
		-- Use stringShift to map field-relative references to indices into a unified string block...
		local sbi, s, sn = {}, 0, 1
		for i=#h.parts, 1, -1 do
			local p = h.parts[i]
			p.stringShift, s = -s - p.stringBase, s + p.rowsLength
		end
		for i=1,#h.parts do
			local p = h.parts[i]
			if p.stringLength > 0 then
				sbi[sn], sbi[sn+1], sn = p.stringLength + (sbi[i+i-3] or 0), p.stringBase - (sbi[i+i-3] or 0), sn+2
			end
		end
		-- ... then convert unified string block indices to indices into the original data.
		if #sbi > 0 then
			sbi[#sbi+1], sbi[#sbi+2] = math.huge, #data+9e12+1
			local lastLow, lastHigh, lastShift, invalid = 0, sbi[1], sbi[2], sbi[#sbi]
			h.mapStringOffset = function(o)
				if o < 0 then
					return invalid
				elseif o < lastLow or o >= lastHigh then
					for i=1,#sbi,2 do
						local si = sbi[i]
						if si > o then
							lastLow, lastHigh, lastShift = sbi[i-2] or 0, si, sbi[i+1]
							break
						end
					end
				end
				return o + lastShift
			end
		end
	end
	if dbcM.PREFER_ERRORS and hasInlineStrings and hasStringBlock then
		error("DC3+ decoder does not support mixing in-line strings and string blocks")
	end
	h.inlineStrings = hasInlineStrings
	
	setFlags(feat,
		packLength > 0 and "PackInfo", codLength > 0 and "DefaultVals",
		palLength > 0 and "EnumFields", h.stringLength > 2 and "StringBlock",
		hasInlineStrings and "InlineStrings", hasInlineStrings and hasStringBlock and "MixedStrings",
		hasForeignKey and "ForeignKeys", h.lookupCount > 0 and "Lookup[" .. h.lookupCount .. "]",
		#h.parts > 1 and ("Parts[" .. #h.parts .. "]")
	)
	keyColumnID = (not hasExternalPrimaryID) and keyColumnID or nil

	local adOffsetPA = packPos and (packPos + packLength)
	local adOffsetCO = packPos and (adOffsetPA+palLength)
	local finfo, extraArrayFields, oddSizedFields, basicArrayFields, narrowedFields = {}, 0, 0, 0, ""
	for i=1, h.fields do
		local fi, sz, ofs = {}, 32-int_le(data, 2, fieldInfoPos-4 + 4*i), uint16_le(data, fieldInfoPos-2 + 4*i)
		finfo[#finfo+1], fi[1], fi[2] = fi, sz/8, ofs
		if packPos then
			fi.bitOffset, fi.bitSize, fi.adLength = uint16_le(data, packPos), uint16_le(data, packPos+2), uint32_le(data, packPos+4)
			fi.packType, fi.pa1, fi.pa2, fi.pa3 = uint32_le(data, packPos+8), uint32_le(data, packPos+12), uint32_le(data, packPos+16), uint32_le(data, packPos+20)
			packPos = packPos + 24
			if not (fi.packType == 0 or sz == 0 or fi.bitSize == sz) then
				error(('DC3+ field/packing width mismatch: field %d, pack %d, sz %d, bsz %d'):format(i, fi.packType, sz, fi.bitSize))
			end
			if fi.packType == 0 and fi.bitSize ~= sz and sz > 0 then
				assert(fi.bitSize % sz == 0, 'DC3+ array field width parity check')
				basicArrayFields = basicArrayFields + (fi.bitSize/sz-1)
				for j=2,fi.bitSize/sz do
					finfo[#finfo+1] = {sz/8, ofs+sz*(j-1)}
				end
			elseif fi.adLength == 0 then
			elseif fi.packType == 2 then
				fi.adOfs, adOffsetCO = adOffsetCO, adOffsetCO + fi.adLength
			else
				assert(fi.packType == 3 or fi.packType == 4, "DC3+ field packing type " .. tostring(fi.packType) .. " not implemented")
				fi.adOfs, adOffsetPA = adOffsetPA, adOffsetPA + fi.adLength
				if fi.adLength > 4 and dbcM.SCAN_PACK_WIDTH then
					local sbyte, p = string.byte, fi.adOfs+4
					local w, b0,c0,d0, b1,c1,d1 = 1, sbyte(data, p-2, p)
					for p=p, adOffsetPA-1, 4 do
						b1,c1,d1 = sbyte(data, p+2, p+4)
						if c1 ~= c0 or d1 ~= d0 then
							w = 4
							break
						elseif b1 ~= b0 then
							w = 2
						end
					end
					fi.packWidth = w
					if w < 4 then
						narrowedFields = (narrowedFields ~= "" and narrowedFields .. ";" or "") .. i .. (w == 1 and "b" or "s")
					end
				end
				if fi.packType == 4 and fi.pa3 >= 1 then
					fi.firstFieldIndex, extraArrayFields = #finfo, extraArrayFields + fi.pa3 - 1
					for i=2, fi.pa3 do
						finfo[#finfo+1] = fi
					end
				end
			end
			oddSizedFields = oddSizedFields + (fi.packType ~= 0 and 1 or 0)
		end
		if i == keyColumnID then
			h.idField = #finfo
		end
	end
	if hasForeignKey then
		finfo[#finfo+1] = {0,0, packType="ForeignKeyMap"}
	end
	feat.ArrayFields = basicArrayFields > 0
	feat.DictArrayFields = extraArrayFields > 0
	feat.PackedFields = oddSizedFields > 0
	feat["NarrowedFields[" .. narrowedFields .. "]"] = narrowedFields ~= "" or nil
	if hasInlineStrings then
		h.rowRelativeStrings = nil
	end
	
	-- update h.fields to match the expanded field count; h.fields2 retains the header value
	h.fieldInfo, h.fields = finfo, #finfo
	h.featureDesc = serializeFlags(feat)
	
	return h
end

return M