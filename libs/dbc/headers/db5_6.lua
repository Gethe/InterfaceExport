-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/dbc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = { fourCC = { WDB5 = 1, WDB6 = 1 } }, require("dbc.bin")
local uint32_le, uint16_le, float32_le, int_le = bin.uint32_le, bin.uint16_le, bin.float32_le, bin.int_le

local function assertLEQ(a, b, message)
	if a > b then
		error(message .. ": " .. a .. " > " .. b, 2)
	end
end

local DB6_coType = {[0]=4, [1]=2, [2]=1, [3]='f', [4]=4, [5]=8}
function M:parseHeader(data)
	local is6 = data:sub(4,4) == '6'
	local h, rkField, idField = {}
	h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
	h.build, h.minId, h.maxId, h.locale = uint32_le(data, 24), uint32_le(data, 28), uint32_le(data, 32), uint32_le(data, 36)
	h.cloneLength, h.flags, rkField = uint32_le(data, 40), uint16_le(data, 44), 1+uint16_le(data, 46)

	local hend = is6 and 56 or 48
	local hsize, ofsSize, idmSize = hend + 4*h.fields, h.flags % 2 > 0 and 6*(h.maxId-h.minId+1) or 0, h.flags % 8 > 3 and 4*h.rows or 0
	local stSize, stringEnd = ofsSize > 0 and 0 or h.stringLength
	assertLEQ(hsize + h.rows*h.stride + stSize + ofsSize + idmSize, #data, "DB5 data too short")

	if ofsSize > 0 and h.rows > 0 then
		local ot, nr, pos, minLen, maxLen, sz = {}, 1, h.stringLength, math.huge, -math.huge
		for i=h.minId, h.maxId do
			pos, sz = pos + 6, uint16_le(data, pos+4)
			if sz > 0 then
				ot[nr], nr = {i, uint32_le(data, pos-6), sz}, nr + 1
				minLen, maxLen = minLen < sz and minLen or sz, maxLen > sz and maxLen or sz
			end
		end
		h.rows, h.rowList, h.maxRowSize, h.minRowSize, h.inlineStrings, stringEnd = #ot, ot, maxLen, minLen, minLen ~= maxLen, h.stringLength + ofsSize
	else
		h.stringBase, stringEnd = hsize + h.rows*h.stride+1, hsize + h.rows*h.stride+stSize
		if idmSize > 0 then
			local idMap, p = {}, h.stringBase+h.stringLength-1
			for i=1,idMap and h.rows or 0 do
				idMap[i], p = uint32_le(data, p), p + 4
			end
			h.idMap = idMap
		end
	end
	h.rowBase, h.cloneOffset = hsize, h.cloneLength > 0 and stringEnd + ofsSize + idmSize or nil

	local finfo, p = {}, hend
	for i=1,h.fields do
		local f, sz, o, no = true, (32-int_le(data, 2, p))/8, uint16_le(data, p+2)
		p, no, idField = p + 4, i == h.fields and h.stride or uint16_le(data, p+6), i == rkField and #finfo+1 or idField
		assert(i == 1 or o >= finfo[#finfo][2]+finfo[#finfo][1])
		repeat
			finfo[#finfo+1], o, f = {sz, o, f}, o + sz, false
		until (o+sz) > no
	end
	h.idField = not h.rowList and not h.idMap and idField or nil
	h.inlineStrings = ofsSize > 0 and (h.maxRowSize ~= h.minRowSize or h.maxRowSize > finfo[#finfo][1]+finfo[#finfo][2]) or nil
	if h.rows > 0 and not finfo[#finfo][3] and finfo[#finfo][1] < 4 then
		local md, rl = 0, h.rowList
		for i=#finfo,1,-1 do
			local f = finfo[i]
			if f[3] then break end
			md = md + f[1]
		end
		for i=1,h.rows do
			local re = rl and (rl[i][2]+rl[i][3]) or (hsize+i*h.stride)
			md = #data:sub(re-md+1,re):match("%z*$")
			if md == 0 then break end
		end
		while md >= finfo[#finfo][1] do
			md, h.dropPadding, finfo[#finfo] = md - finfo[#finfo][1], (h.dropPadding or 0) + finfo[#finfo][1]
		end
	end

	local coSize = is6 and uint32_le(data, 52) or 0
	if coSize > 0 then
		local _totalFields = uint32_le(data, 48)
		local cobase, p, nc = stringEnd + ofsSize + idmSize + h.cloneLength
		h.coVals, h.coTypes, p, nc = {}, '', cobase + 4, uint32_le(data, cobase)
		for i=1, nc do
			local n, t, c = uint32_le(data, p), data:byte(p+5), {}
			h.coVals[i], c.type, p = c, t, p + 5
			local tt = DB6_coType[t]
			if tt == 'f' then
				c.ttype = 'f'
				for i=1,n do
					c[uint32_le(data, p)], p = float32_le(data, p+4), p + 8
				end
			else
				c.ttype = 'i'
				for i=1,n do
					c[uint32_le(data, p)], p = int_le(data, tt, p+4), p + 4 + tt
				end
			end
			h.coTypes = h.coTypes .. c.ttype
		end
	end

	h.fieldInfo, h.fields = finfo, #finfo
	return h
end

return M