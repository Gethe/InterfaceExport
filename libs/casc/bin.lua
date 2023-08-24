-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, sbyte, schar, sgsub, sformat, ssub = {}, string.byte, string.char, string.gsub, string.format, string.sub
local inf, nan, floor, min = math.huge, math.huge-math.huge, math.floor, math.min
local MAX_SLICE_SIZE = 4096
local CM do
	local ok, m = pcall(require, "casc.binc")
	CM = ok and m
end

local hexbin = {} for i=0,255 do
	local h, b = sformat("%02x", i), schar(i)
	hexbin[sformat("%02X", i)], hexbin[h], hexbin[b] = b, b, h
end

local PRECISION_BITS, PRECISION_BYTES, PRECISION_SAFE1_BYTES, PRECISION_SAFE1_BITS do
	local x = 255
	for i=8,128 do
		local x1 = x-1
		local x2 = x1-1
		if not (x > 128 and x > x1 and x1 > x2 and (x2+2) == x and (x1+1) == x and (2^i-x) == 1) then
			break
		else
			PRECISION_BITS, x = i, x*2+1
		end
	end
	PRECISION_BYTES = (PRECISION_BITS - PRECISION_BITS%8)/8
	M.PRECISION_BITS, M.PRECISION_BYTES = PRECISION_BITS, PRECISION_BYTES
	PRECISION_SAFE1_BYTES = PRECISION_BYTES-1
	PRECISION_SAFE1_BITS = PRECISION_SAFE1_BYTES * 8
end

local function uint_le(s, n, pos)
	if n > PRECISION_BYTES then
		error('Requested integer is too wide: ' .. n .. ' bytes of precision required; ' .. PRECISION_BITS .. ' bits available.', 2)
	end
	local a, b, c, d, e, f = sbyte(s, (pos or 0)+1, (pos or 0)+n)
	return (f or 0)*256^5 + (e or 0)*256^4 + (d or 0)*256^3 + (c or 0)*256^2 + (b or 0)*256 + a + (n > 6 and 2^48*uint_le(s,n-6,pos+6) or 0)
end
local function int_le(s, n, pos)
	if n > PRECISION_BYTES then
		error('Requested integer is too wide: ' .. n .. ' bytes of precision required; have ' .. PRECISION_BITS .. ' bits', 2)
	end
	local a, b, c, d, e, f = sbyte(s, (pos or 0)+1, (pos or 0)+n)
	local r = (f or 0)*256^5 + (e or 0)*256^4 + (d or 0)*256^3 + (c or 0)*256^2 + (b or 0)*256 + a + (n > 6 and 2^48*uint_le(s,n-6,pos+6) or 0)
	local lb = n <= 6 and (f or e or d or c or b or a) or sbyte(s, (pos or 0)+n)
	return r - (lb > 127 and 256^n or 0)
end
local function upint_le(s, w, pos)
	if w > PRECISION_BITS then
		error('Requested packed integer is too wide: ' .. w .. ' bits of precision required; ' .. PRECISION_BITS .. ' bits available.', 2)
	end
	local o = 0
	pos = pos or 0
	if w > PRECISION_SAFE1_BITS then
		w, o = PRECISION_SAFE1_BITS, 2^PRECISION_SAFE1_BITS*upint_le(s, w-PRECISION_SAFE1_BITS, pos+PRECISION_SAFE1_BITS)
	end
	local p8 = pos % 8
	local lo, iv = 2^p8, uint_le(s, PRECISION_BYTES, (pos-p8)/8)
	return o + ((iv - iv % lo)/lo % 2^w)
end
local function pint_le(s, w, pos)
	local o = upint_le(s, w, pos)
	return o - (o >= 2^(w-1) and 2^w or 0)
end
M.uint_le, M.int_le = uint_le, int_le
M.upint_le, M.pint_le = upint_le, pint_le

function M.uint16_le(s, pos)
	local a, b = sbyte(s, (pos or 0)+1, (pos or 0) + 2)
	return b*256 + a
end
function M.uint32_le(s, pos)
	local a, b, c, d = sbyte(s, (pos or 0)+1, (pos or 0) + 4)
	return d*256^3 + c*256^2 + b*256 + a
end
function M.uint16_be(s, pos)
	local a,b = sbyte(s, (pos or 0)+1, (pos or 0) + 2)
	return a*256 + b
end
function M.uint32_be(s, pos)
	local a,b,c,d = sbyte(s, (pos or 0)+1, (pos or 0) + 4)
	return a*256^3 + b*256^2 + c*256 + d
end
function M.uint40_be(s, pos)
	local a, b, c, d, e = sbyte(s, (pos or 0)+1, (pos or 0) + 5)
	return a*256^4 + b*256^3 + c*256^2 + d*256 + e
end
function M.float32_le(s, pos)
	local a, b, c, d = sbyte(s, (pos or 0) + 1, (pos or 0) + 4)
	local s, e, f = d > 127 and -1 or 1, (d % 128)*2 + (c > 127 and 1 or 0), a + b*256 + (c % 128)*256^2
	if e > 0 and e < 255 then
		return s * (1+f/2^23) * 2^(e-127)
	else
		return e == 0 and (s * f/2^23 * 2^-126) or f == 0 and (s * inf) or nan
	end
end
function M.int32_le(s, pos)
	local a, b, c, d = sbyte(s, (pos or 0)+1, (pos or 0) + 4)
	return (d or 0)*256^3 + (c or 0)*256^2 + (b or 0)*256 + a - (d > 127 and 2^32 or 0)
end
function M.int64ish_le(s, pos)
	local a, b, c, d, e, f, g, h = sbyte(s, (pos or 0)+1, (pos or 0) + 8)
	return ((h % 128) * 256^7 + g * 256^6 + f*256^5 + e*256^4 + d*256^3 + c*256^2 + b*256 + a) * (h > 127 and -1 or 1)
end

function M.to_le32(n)
	local n = n % 2^32
	return schar(floor(n) % 256, floor(n / 256) % 256, floor(n / 256^2) % 256, floor(n / 256^3) % 256)
end

function M.to_bin(hs)
	return hs and sgsub(hs, "%x%x", hexbin)
end
function M.to_hex(bs)
	return bs and sgsub(bs, ".", hexbin)
end

M.sadd = CM and CM.sadd or function(a, ap, b, bp, length, out, on)
	local bsz, rx, unpack = #b, length, unpack or table.unpack
	while rx > 0 do
		if bp > bsz then
			out[on], on, bp, ap, rx = ssub(a, ap, ap+rx-1), on+1, bp + rx, ap + rx, 0
		else
			local slice = min(rx, bsz-bp+1, MAX_SLICE_SIZE)
			local t1, t2 = {sbyte(b, bp, bp+slice-1)}, {sbyte(a, ap, ap+slice-1)}
			for i=1,slice do
				t1[i] = (t1[i] + t2[i]) % 256
			end
			out[on], on, ap, bp, rx = schar(unpack(t1)), on+1, ap + slice, bp + slice, rx - slice
		end
	end
	return on
end

return M