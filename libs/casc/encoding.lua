-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = {}, require("casc.bin")
local uint32_be, uint16_le, ssub = bin.uint32_be, bin.uint16_le, string.sub

local encoding_mt = {} do
	local api = {}
	local function parseContentChunk(data, chunk, into)
		local p, last, firstKey = chunk[3]-1, chunk[3] + 4059, chunk[1]
		repeat
			local c, chsh = uint16_le(data, p), ssub(data, p+7, p+22)
			into[chsh], p = c > 0 and {} or nil, p + 22
			for i=1, c do
				into[chsh][i], p = ssub(data, p+1, p+16), p + 16
			end
		until p > last or chsh < firstKey
	end
	local function searchChunks(data, chunks, key, parse, into)
		if chunks.unloaded > 0 then
			local l, h = 1, #chunks
			while l ~= h do
				local m = math.ceil((l + h)/2)
				if chunks[m][1] <= key then
					l = m
				else
					h = m - 1
				end
			end
			local chunk = chunks[l]
			if not chunk[4] then
				chunk[4], chunks.unloaded = true, chunks.unloaded - 1
				parse(data, chunk, into)
				return into[key]
			end
		end
	end
	function api:getEncodingHash(rawContentHash)
		local dt = self._data
		local cache = dt.ccache
		return cache[rawContentHash] or searchChunks(dt.data, dt.content, rawContentHash, parseContentChunk, cache)
	end
	function encoding_mt:__tostring()
		return ("CASC:encoding (%d/%d c/d-chunks)"):format(#self._data.content, #self._data.encoding)
	end
	encoding_mt.__index = api
end

function M.parse(data)
	assert(type(data) == "string")
	assert(ssub(data,1,2) == "EN", "encoding magic mismatch")

	local numContentChunks = uint32_be(data, 9)
	local numEncodingChunks = uint32_be(data, 13)
	local recipeSize = uint32_be(data, 18)
	
	local recipes = {}
	local p2, rid = 23, 0 repeat
		rid, recipes[rid], p2 = rid + 1, data:match("^(%Z+)%z()", p2)
	until p2 > 22 + recipeSize
	
	local contentChunks, p2 = {unloaded=numContentChunks}, 23 + recipeSize
	local cbase = p2 + 32 * numContentChunks
	for i=1,numContentChunks do
		contentChunks[i], p2 = {ssub(data, p2, p2+15), p2+16, cbase+(i-1)*4096}, p2 + 32
	end
	
	local encodingChunks, p2 = {unloaded=numEncodingChunks}, cbase + 4096*numContentChunks
	local ebase = p2 + 32 * numEncodingChunks
	for i=1,numEncodingChunks do
		encodingChunks[i], p2 = {ssub(data, p2, p2+15), p2+16, ebase+(i-1)*4096}, p2 + 32
	end
	
	local _data = {data=data, recipes=recipes, content=contentChunks, encoding=encodingChunks}
	_data.ccache, _data.ecache = {}, {}
	return setmetatable({_data=_data}, encoding_mt)
end

return M