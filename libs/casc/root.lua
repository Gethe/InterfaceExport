local M = {}

local jenkins96, bin = require("casc.jenkins96"), require("casc.bin")
local uint32_le, int32_le, to_bin, ssub = bin.uint32_le, bin.int32_le, bin.to_bin, string.sub

local function toBinHash(h, hexLength)
	return #h == hexLength and to_bin(h) or h
end

local wow_mt = {__index={}} do
	local wow = wow_mt.__index
	local function nextVariant(e, i)
		i = (i or -1) + 2
		local h = e and e[i]
		if h then
			return i, h, e[i+1]
		end
	end
	function wow:getFileVariants(nameOrID)
		local fid = nameOrID
		if type(nameOrID) ~= "number" then
			fid = self:getFileID(nameOrID)
		end
		return nextVariant, self[2][fid]
	end
	function wow:addFileVariant(path, chash, flags)
		local nameMap, idVariantMap = self[1], self[2]
		local hpath = jenkins96.hash_path(path)
		local fid = nameMap[hpath]
		if not fid then
			fid = (self[3] or 10041)-1
			nameMap[hpath], idVariantMap[fid], self[3] = fid, {}, fid
		end
		local t = idVariantMap[fid]
		t[#t+1], t[#t+2] = toBinHash(chash, 32), flags
	end
	function wow:getFileID(path)
		local h = jenkins96.hash_path(path)
		local fid = self[1][h]
		fid = fid or (self.pathFileIDLookup and self.pathFileIDLookup(path, h)) or nil
		return fid
	end
	function wow:addFileIDPaths(map, pathsArePrehashed)
		local nameMap, idVariantMap = self[1], self[2]
		for k,v in pairs(map) do
			local tk, tv, path, fileID = type(k), type(v)
			if tk == "number" and tv == "string" then
				path, fileID = v, k
			elseif tk == "string" and tv == "number" then
				path, fileID = k, v
			end
			if not path then
				error("root:addFileIDPaths: map kv pairs should be (string, number) or (number, string).")
			end
			local h = pathsArePrehashed and to_bin(path, 16) or jenkins96.hash_path(path)
			if nameMap[h] == nil and idVariantMap[fileID] then
				nameMap[h] = fileID
			end
		end
	end
end

local function parseLegacy(data)
	local pos, dl, nameMap, idVariantMap = 0, #data, {}, {}
	while pos < dl do
		if dl < (pos+12) then
			return false, 'Root file invalid: expected to read at least 12 bytes from position ' .. pos
		end
		local n, info = uint32_le(data, pos), {uint32_le(data, pos+4), uint32_le(data, pos+8)}
		local p2, lfid = pos + 12, 0
		if dl < (p2+28*n) then
			return false, 'Root file invalid: expected to read at least ' .. (28*n) .. ' bytes from position ' .. p2
		end
		pos = p2 + 4*n
		for i=1,n do
			local chash, tfid = ssub(data, pos+1, pos+16), lfid+int32_le(data, p2)
			local nhash = ssub(data, pos+17, pos+24)
			pos, p2 = pos + 24, p2 + 4
			local t, tsz = idVariantMap[tfid] or {}
			if (nameMap[nhash] or tfid) ~= tfid then
				return false, 'Root manifest invalid: file name name ' .. ('%02x'):rep(#nhash):format(nhash:byte(1, #nhash)) .. ' maps to multiple file IDs'
			end
			nameMap[nhash], idVariantMap[tfid], tsz = tfid, t, #t
			t[tsz+1], t[tsz+2], lfid = chash, info, tfid + 1
		end
	end
	return setmetatable({nameMap, idVariantMap}, wow_mt)
end

local function parseMFST(data)
	if #data < 16 then
		return false, 'Root manifest invalid: expected to read at least 16 bytes from position 0'
	end
	local pos, dl, nameMap, idVariantMap = 12, #data, {}, {}
	local readFiles, readNamedFiles = 0, 0
	local two28, two29 = 2^28, 2^29
	while pos < dl do
		if dl < (pos+12) then
			return false, 'Root manifest invalid: expected to read at least 12 bytes from position ' .. pos
		end
		local n, info = uint32_le(data, pos), {uint32_le(data, pos+4), uint32_le(data, pos+8)}
		local p2, p3, lfid = pos + 12, pos + 12 + 20*n, 0
		local hasNameHashes = (info[1] % two29 < two28) and 1
		if dl < (p2+(hasNameHashes and 28 or 20)*n) then
			return false, 'Root manifest invalid: expected to read at least ' .. (28*n) .. ' bytes from position ' .. p2
		end
		pos = p2 + 4*n
		for i=1,n do
			local chash, tfid = ssub(data, pos+1, pos+16), lfid+int32_le(data, p2)
			pos, p2 = pos + 16, p2 + 4
			local t, tsz = idVariantMap[tfid] or {}
			if hasNameHashes then
				local nhash = ssub(data, p3+1, p3+8)
				if (nameMap[nhash] or tfid) ~= tfid then
					return false, 'Root manifest invalid: file name name ' .. ('%02x'):rep(#nhash):format(nhash:byte(1, #nhash)) .. ' maps to multiple file IDs'
				end
				nameMap[nhash], p3 = tfid, p3 + 8
			end
			idVariantMap[tfid], tsz = t, #t
			t[tsz+1], t[tsz+2], lfid = chash, info, tfid + 1
			readFiles, readNamedFiles = readFiles + 1, readNamedFiles + (hasNameHashes or 0)
		end
		pos = p3
	end
	if readFiles ~= uint32_le(data, 4) then
		return false, 'Root manifest invalid: expected ' .. uint32_le(data, 4) .. ' files; found ' .. readFiles
	end
	if readNamedFiles ~= uint32_le(data, 8) then
		return false, 'Root manifest invalid: expected ' .. uint32_le(data, 8) .. ' named files; found ' .. readNamedFiles
	end
	return setmetatable({nameMap, idVariantMap}, wow_mt)
end

function M.parse(data)
	if data:sub(1,4) == "TSFM" then
		return parseMFST(data)
	end
	return parseLegacy(data)
end
function M.empty()
	return setmetatable({{}, {}}, wow_mt)
end

return M