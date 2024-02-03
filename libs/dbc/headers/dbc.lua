-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/dbc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = { fourCC = { WDBC = 1 } }, require("dbc.bin")
local uint32_le = bin.uint32_le

local function assertLEQ(a, b, message)
	if a > b then
		error(message .. ": " .. a .. " > " .. b, 2)
	end
end

function M:parseHeader(data)
	local h = {rowBase=20}
	h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
	assertLEQ(20 + h.rows*h.stride + h.stringLength, #data, "DBC data too short")
	h.stringBase = h.rowBase + h.rows*h.stride + 1
	return h
end

return M