-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, decompress, bin = {}, require("casc.platform").decompress, require("casc.bin")
local CONCAT_CHUNK_SIZE, CONCAT_STOP_LENGTH = 512, 16384

function M.patch(old, patch)
	local ssub, int64ish_le, sadd = string.sub, bin.int64ish_le, bin.sadd
	
	if ssub(patch, 1, 8) ~= "ZBSDIFF1" then
		return nil, "corrupt patch: signature mismatch"
	end
	
	local csz, dsz, nsz = int64ish_le(patch, 8), int64ish_le(patch, 16), int64ish_le(patch, 24)
	if #patch < 32 + csz + dsz then
		return nil, "corrupt patch: header size mismatch"
	end
	
	local control = decompress(ssub(patch, 33, 32+csz))
	local data = decompress(ssub(patch, 33+csz, 32+csz+dsz))
	local extra = decompress(ssub(patch, 33+csz+dsz))
	local o, on, oh, op, dp, ep, np = {}, 1, 1, 1,1,1, 0
	for p=0,#control-1,24 do
		local x, y = int64ish_le(control, p), int64ish_le(control, p+8)
		if x < 0 or y < 0 then
			return nil, "corrupt patch: negative block length"
		elseif np + x + y > nsz then
			return nil, "corrupt patch: overflows declared size"
		elseif #data < dp + x - 1 then
			return nil, "corrupt patch: overread data"
		elseif #extra < ep + y - 1 then
			return nil, "corrupt patch: overread extra"
		end
		if x > 0 then
			dp, op, on = dp + x, op + x, sadd(data, dp, old, op, x, o, on)
		end
		if y > 0 then
			o[on], on, ep = ssub(extra, ep, ep + y - 1), on+1, ep + y
		end
		if oh + CONCAT_CHUNK_SIZE < on then
			o[oh], on = table.concat(o, "", oh, on-1), oh + 1
			if #o[oh] > CONCAT_STOP_LENGTH then oh = oh + 1 end
		end
		op, np = op + int64ish_le(control, p+16), np + x + y
	end
	
	if np ~= nsz then
		return nil, "corrupt patch: underflows declared size"
	end
	
	return table.concat(o, "", 1, on-1)
end

return M