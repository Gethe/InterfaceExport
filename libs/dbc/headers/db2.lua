-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/dbc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = { fourCC = { WDB2 = 1, WCH2 = 1 } }, require("dbc.bin")
local uint32_le = bin.uint32_le

local function assertLEQ(a, b, message)
	if a > b then
		error(message .. ": " .. a .. " > " .. b, 2)
	end
end

function M:parseHeader(data)
	local h = {rowBase=48}
	h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
	h.build, h.minId, h.maxId, h.locale = uint32_le(data, 24), uint32_le(data, 32), uint32_le(data, 36), uint32_le(data, 40)

	if h.maxId > 0 then
		local n, p, idMap = h.maxId-h.minId + 1, h.rowBase, {}
		h.idMap, h.rowBase = idMap, h.rowBase + 6 * n
		for i=1,n do
			idMap[i], p = uint32_le(data, p), p + 6
		end
	end
	assertLEQ(h.rowBase + h.rows*h.stride + h.stringLength, #data, "DB2 data too short")
	h.stringBase = h.rowBase + h.rows*h.stride + 1

	return h
end

return M