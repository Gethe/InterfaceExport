-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/dbc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = { fourCC = { WDC1=1, WDC2=1, ["1SLC"]=1 } }, require("dbc.bin")
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

local identityMap = setmetatable({}, {__index=function(_,k) return k end})
local dc1Map = { [40]=44, [42]=46, [44]=48, [48]=52, [52]=56, [56]=68, [60]=72, [64]=76, [92]=40, [96]=60, [100]=64, [104]=80 }
function M:parseHeader(data)
	local isDC1 = data:sub(4,4) == '1'
	local h, om, feat = {}, isDC1 and dc1Map or identityMap, {}
	h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
	h.minId, h.maxId, h.locale = uint32_le(data, 28), uint32_le(data, 32), uint32_le(data, 36)
	h.flags = uint16_le(data, om[40])
	h.fields2, h.packStart, h.lookupCount = uint32_le(data, om[44]), uint32_le(data, om[48]), uint32_le(data, om[52])
	local cloneLength = uint32_le(data, om[92])
	local keyColumnID = 1+uint16_le(data, om[42])
	local rowListOffset = uint32_le(data, om[96])
	local idmLength = uint32_le(data, om[100])
	local packLength = uint32_le(data, om[56])
	local codLength = uint32_le(data, om[60])
	local palLength = uint32_le(data, om[64])
	local relLength = uint32_le(data, om[104])
	local staticHeaderEnd = (isDC1 and 84 or 108)
	local headerEnd = staticHeaderEnd + 4*h.fields
	h.rowBase = isDC1 and headerEnd or uint32_le(data, 80)

	h.stringBase = h.rowBase + h.rows*h.stride + 1
	setFlags(feat,
		packLength > 0 and "PackInfo", idmLength > 0 and "RowIDMap", codLength > 0 and "DefaultVals",
		palLength > 0 and "EnumFields", relLength > 0 and "ForeignKeys", rowListOffset > 0 and "OffsetMap",
		cloneLength > 0 and "CloneRows", h.stringLength > 2 and "StringBlock"
	)
	keyColumnID = idmLength == 0 and keyColumnID or nil

	local mainDataEnd = rowListOffset == 0 and (h.rowBase + h.rows*h.stride + h.stringLength) or (rowListOffset + (h.maxId - h.minId+1)*6)
	local auxDataOffset = mainDataEnd+idmLength+cloneLength+packLength
	local auxDataEnd = auxDataOffset+palLength+codLength
	local fileEnd = auxDataEnd + relLength
	if not isDC1 then
		fileEnd = mainDataEnd + idmLength + cloneLength + relLength
		if h.stringLength > 0 then
			h.rowRelativeStrings = true
		end
	end

	assert(h.fields == h.fields2, "DC1 header fields/fields2 values are inconsistent")
	assertLEQ(fileEnd, #data, "DC1 data too short")
	assert(isDC1 or uint32_le(data, 68) == 1, "DC2 multi-part tables are not supported")

	assert(packLength == 0 or packLength/24 == h.fields, "DC1 packing data size invalid")
	local packPos = packLength > 0 and (isDC1 and (mainDataEnd + idmLength + cloneLength) or (headerEnd))
	local adOffsetPA = packPos and (isDC1 and (auxDataOffset) or (packPos + packLength))
	local adOffsetCO = packPos and (adOffsetPA+palLength)
	local finfo, extraArrayFields, oddSizedFields, basicArrayFields = {}, 0, 0, 0
	for i=1, h.fields do
		local fi, sz, ofs = {}, 32-int_le(data, 2, staticHeaderEnd-4 + 4*i), uint16_le(data, staticHeaderEnd-2 + 4*i)
		finfo[#finfo+1], fi[1], fi[2] = fi, sz/8, ofs
		if packPos then
			fi.bitOffset, fi.bitSize, fi.adLength = uint16_le(data, packPos), uint16_le(data, packPos+2), uint32_le(data, packPos+4)
			fi.packType, fi.pa1, fi.pa2, fi.pa3 = uint32_le(data, packPos+8), uint32_le(data, packPos+12), uint32_le(data, packPos+16), uint32_le(data, packPos+20)
			packPos = packPos + 24
			if not (fi.packType == 0 or sz == 0 or fi.bitSize == sz) then
				error(('DC1 field/packing width mismatch: field %d, pack %d, sz %d, bsz %d'):format(i, fi.packType, sz, fi.bitSize))
			end
			if fi.packType == 0 and fi.bitSize ~= sz and sz > 0 then
				assert(fi.bitSize % sz == 0, 'DC1 array field width parity check')
				basicArrayFields = basicArrayFields + (fi.bitSize/sz-1)
				for j=2,fi.bitSize/sz do
					finfo[#finfo+1] = {sz/8, ofs+sz*(j-1)}
				end
			elseif fi.adLength == 0 then
			elseif fi.packType == 2 then
				fi.adOfs, adOffsetCO = adOffsetCO, adOffsetCO + fi.adLength
			else
				assert(fi.packType == 3 or fi.packType == 4, "Unknown DC1 field packing type: " .. fi.packType)
				fi.adOfs, adOffsetPA = adOffsetPA, adOffsetPA + fi.adLength
				if fi.packType == 4 and fi.pa3 >= 1 then
					fi.firstFieldIndex, extraArrayFields = #finfo, extraArrayFields + fi.pa3 - 1
					for i=2,fi.pa3 do
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
	feat.ArrayFields = basicArrayFields > 0
	feat.DictArrayFields = extraArrayFields > 0
	feat.PackedFields = oddSizedFields > 0

	if cloneLength > 0 then
		assert(cloneLength % 8 == 0, 'DC1 clone instructions length parity check')
		h.cloneOffset, h.cloneLength = mainDataEnd + idmLength, cloneLength
	end
	if relLength > 0 then
		local p = isDC1 and auxDataEnd or (mainDataEnd + idmLength + cloneLength)
		local count, min, max = uint32_le(data, p), uint32_le(data, p+4), uint32_le(data, p+8)
		h.fkField = {0,0, packType="ForeignKeyMap", adOfs=p+12, adLength=relLength-12, pa1=count, pa2=min, pa3=max}
		finfo[#finfo+1] = h.fkField
	end

	-- We don't treat arrays as single fields around here, so lie about the field count in the header
	-- (h.fields2 preserves the original count)
	h.fieldInfo, h.fields = finfo, #finfo

	if rowListOffset > 0 and h.rows > 0 then
		local ot, nr, pos, minLen, maxLen, sz = {}, 1, rowListOffset, math.huge, -math.huge
		for i=h.minId, h.maxId do
			pos, sz = pos + 6, uint16_le(data, pos+4)
			if sz > 0 then
				ot[nr], nr = {i, uint32_le(data, pos-6), sz}, nr + 1
				minLen, maxLen = minLen < sz and minLen or sz, maxLen > sz and maxLen or sz
			end
		end
		h.rows, h.rowList, h.maxRowSize, h.minRowSize, h.inlineStrings = #ot, ot, maxLen, minLen, minLen ~= maxLen
		feat.InlineStrings = h.inlineStrings
	elseif idmLength > 0 then
		local idMap, p = {}, mainDataEnd
		for i=1, h.rows do
			idMap[i], p = uint32_le(data, p), p + 4
		end
		h.idMap = idMap
	end
	h.featureDesc = serializeFlags(feat)

	if h.inlineStrings then
		h.rowRelativeStrings = nil
	end

	return h
end

return M