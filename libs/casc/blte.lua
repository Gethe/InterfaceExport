-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, plat, bin = {}, require("casc.platform"), require("casc.bin")
local uint32_le, uint32_be, decompress = bin.uint32_le, bin.uint32_be, plat.decompress

local string_cursor do
	local function read(self, n)
		local p = self.pos
		self.pos = p + n
		return self.str:sub(p, p+n-1)
	end
	local function seek(self, dir, n)
		assert(dir == 'cur', 'String cursor only supports relative seeks')
		self.pos = self.pos + n
	end
	function string_cursor(s)
		return {str=s, read=read, seek=seek, pos=1}
	end
end
local function closeAndReturn(h, ...)
	h:close()
	return ...
end

local salsa20 do
	local UINT32_TRUNCATE, R = 2^32, {
		  4,0,12,7,    8,4,0,9,   12,8,4,13,   0,12,8,18,
		   9,5,1,7,   13,9,5,9,   1,13,9,13,   5,1,13,18,
		 14,10,6,7,  2,14,10,9,   6,2,14,13,   10,6,2,18,
		 3,15,11,7,   7,3,15,9,   11,7,3,13,  15,11,7,18,
		   1,0,3,7,    2,1,0,9,    3,2,1,13,    0,3,2,18,
		   6,5,4,7,    7,6,5,9,    4,7,6,13,    5,4,7,18,
		 11,10,9,7,  8,11,10,9,   9,8,11,13,   10,9,8,18,
		12,15,14,7, 13,12,15,9, 14,13,12,13, 15,14,13,18}
	function salsa20(data, key, iv, s, e)
		local bxor, char, byte = plat.bxor, string.char, string.byte
		local lit = #key == 16 and "expand 16-byte k" or "expand 32-byte k"
		local k2, k = {}, {[0]=
			uint32_le(lit, 0), uint32_le(key, 0), uint32_le(key, 4), uint32_le(key, 8),
			uint32_le(key, 12), uint32_le(lit, 4), uint32_le(iv, 0), uint32_le(iv, 4),
			0, 0, uint32_le(lit, 8), uint32_le(key, #key-16),
			uint32_le(key, #key-12), uint32_le(key, #key-8), uint32_le(key, #key-4), uint32_le(lit, 12)
		}
		local function syn(ob, ki, at)
			local k4, a, b, c, d = k2[ki], byte(data, at, at+3)
			local k1, k2, k3 = k4 % 0x100, k4 % 0x10000, k4 % 0x1000000
			a = a and bxor(a, k1)
			b = b and bxor(b, (k2-k1)/0x100)
			c = c and bxor(c, (k3-k2)/0x10000)
			d = d and bxor(d, (k4-k3)/0x1000000)
			if ob > 4 then
				return a, b, c, d, syn(ob-4, ki+1, at+4)
			elseif ob == 4 then
				return a, b, c, d
			elseif ob == 3 then
				return a, b, c
			elseif ob == 2 then
				return a, b
			elseif ob == 1 then
				return a
			end
		end

		local o, oc, nb = {}, 1, e-s+1
		for p=s, e, 64 do
			for i=0,15 do
				k2[i] = k[i]
			end
			for i=0,18,2 do
				for j=1,#R,4 do
					local x, rl = R[j], R[j+3]
					local hiPow, loPow, s = 2^(32-rl), 2^rl, (k2[R[j+1]] + k2[R[j+2]]) % UINT32_TRUNCATE
					local r = s % hiPow
					k2[x] = bxor(k2[x], r * loPow + (s - r)/hiPow)
				end
			end
			for i=0,15 do
				k2[i] = (k2[i] + k[i]) % UINT32_TRUNCATE
			end
			o[#o+1] = char(syn(nb > 64 and 64 or nb, 0, p))
			k[8] = k[8] + 1
			if k[8] == UINT32_TRUNCATE then
				k[8], k[9] = 0, k[9]+1
			end
			if #o == 1023 then
				o[oc] = table.concat(o, "", oc)
				for i=#o,oc + 1,-1 do o[i] = nil end
				oc = (oc % 512) + 1
			end
			nb = nb - 64
		end
		return table.concat(o, "")
	end
end

local function decodeChunk(chunk, s, e, idx, keys)
	local format = chunk:sub(s, s)
	if format == 'N' then
		return chunk:sub(s+1, e)
	elseif format == 'Z' then
		local dc, er = decompress(chunk:sub(s+1, e))
		if not dc or #dc == 0 then
			return nil, 'BLTE: chunk decompression failed' .. (er and ': ' .. er or '')
		end
		return dc
	elseif format == 'E' then
		local knsz = chunk:byte(s+1)
		local ivsz = chunk:byte(s+2+knsz)
		local algo = chunk:byte(s+3+knsz+ivsz)
		if s+3+knsz+ivsz >= e then
			return nil, 'BLTE: Encrypted chunk is too short'
		elseif algo == 83 then
			local kname = chunk:sub(s+2, s+1+knsz):reverse()
			local key = type(keys) == "table" and keys[kname]
			if not key then
				return nil, 'BLTE: missing encryption key Kx' .. bin.to_hex(kname), 'missing-key'
			end
			local iv = chunk:sub(s+3+knsz, s+2+knsz+ivsz)
			iv = (iv .. ("\0"):rep(8-#iv)):gsub("()(.)", function(p,c)
				return string.char(plat.bxor(c:byte(), math.floor(idx/256^(p-1)) % 256))
			end)
			local o, err = salsa20(chunk, key, iv, s+4+knsz+ivsz, e)
			if not o then return o, err end
			return decodeChunk(o, 1, #o, idx, keys)
		else
			return nil, ('BLTE: Unsupported chunk format: Ex%02x'):format(algo or 1028)
		end
	else
		return nil, 'BLTE: unknown chunk format: ' .. tostring(format)
	end
end
local function parseBLTE(h, dataSize, keys, opts)
	local hadZerofilledContent = false
	local header = h:read(8)
	if type(header) ~= "string" or header:sub(1,4) ~= 'BLTE' then
		local err = type(header)
		if err == "string" then
			err = #header == 0 and "empty string" or ('%02x'):rep(math.min(#header, 4)):format(header:byte(1,4))
		end
		return nil, 'BLTE: expected header signature; got ' .. err
	end
	local ofs = uint32_be(header, 4)
	
	local chunks, ret, err, err2 = ofs > 0 and {}
	if ofs > 0 then
		local sz = h:read(4)
		local buf, cn = h:read(uint32_be(sz) % 2^16 * 24), 1
		header = header .. sz .. buf
		if #header > ofs then
			return nil, 'BLTE: header overread'
		end
		h:seek("cur", ofs-#header)
		for p=0, #buf-1, 24 do
			local esz, dsz, i = uint32_be(buf, p), uint32_be(buf, p+4), cn
			cn, chunks[cn], err, err2 = cn+1, decodeChunk(h:read(esz), 1, esz, cn-1, keys)
			if err2 == 'missing-key' and opts and opts.zerofillEncryptedChunks then
				hadZerofilledContent = true
				chunks[i] = ('\0'):rep(dsz)
				if opts.log then
					opts.log('WARN', err, 'Zero-filled chunk ' .. (cn-1) .. ' (' .. dsz .. ' bytes)')
				end
			end
			if not chunks[cn-1] then
				return nil, err
			end
		end
		ret = table.concat(chunks, "")
	else
		local chunk = h:read(dataSize-8)
		header, ret, err, err2 = header .. chunk, decodeChunk(chunk, 1, -1, 0, keys)
	end
	if not ret then
		hadZerofilledContent = nil
	end
	return ret, ret and header or err, hadZerofilledContent
end

function M.readArchive(path, offset, keys, opts)
	assert(type(path) == "string" and type(offset) == "number" and (type(keys) == "table" or not keys) and (type(opts) == "table" or opts == nil), 'Syntax: "content", "header" = blte.readArchive("path", offset[, keyring][, options])')
	assert(opts == nil or opts.log == nil or type(opts.log) == "function", "blte.readArchive: if specified, options.log must be a function")

	local h, err = io.open(path, "rb")
	if not h then
		return nil, err
	end
	h:seek("set", offset)
	
	return closeAndReturn(h, parseBLTE(h, uint32_le(h:read(30), 16)-30, keys, opts))
end
function M.readData(str, keys, opts)
	assert(type(str) == "string" and (type(keys) == "table" or not keys), 'Syntax: "content", "header" = blte.readData("data"[, keyring][, options])')
	assert(opts == nil or opts.log == nil or type(opts.log) == "function", "blte.readData: if specified, options.log must be a function")
	
	return parseBLTE(string_cursor(str), #str, keys, opts)
end

local function hexbyte(c)
	return string.char(tonumber(c,16))
end
function M.newKeyRing(keys, skipModuleKeys)
	local ret, dk = {}
	if not skipModuleKeys then
		local ok, defKeys = pcall(require, "casc.keys")
		if not (type(defKeys) == 'table' or not ok) then
			return nil, 'blte.newKeyRing: casc.keys module, if available, must return a table'
		end
		dk = ok and defKeys
	end
	local kt = dk or keys
	for i=1,dk and keys and 2 or 1 do
		if kt ~= nil then
			assert(type(kt) == "table", 'blte.newKeyRing: table expected, got ' .. type(kt))
			for k,v in pairs(kt) do
				if not (type(k) == "string" and type(v) == "string" and k:match("^[%x%s]+$") and v:match("^[%x%s]+$")) then
					return nil, ('Invalid key table entry: %q=%q (%d)'):format(tostring(k), tostring(v), i)
				end
				ret[k:gsub("%s*(%x%x)%s*", hexbyte)] = v:gsub("%s*(%x%x)%s*", hexbyte)
			end
		end
		kt = keys
	end
	return ret
end

return M