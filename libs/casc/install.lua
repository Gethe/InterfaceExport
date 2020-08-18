local M, bin = {}, require("casc.bin")
local byte = string.byte

function M.parseInstall(installData)
	assert(type(installData) == "string", 'Syntax: casc.install.parseInstall("installData")')
	assert(installData:sub(1,2) == "IN", 'missing install header magic')
	local numFiles, numCategories = bin.uint32_be(installData, 6), bin.uint16_be(installData, 4)
	
	local categories, cbpos, pos, bitfieldLength = {}, {}, 11, math.ceil(numFiles/8)
	for i=1, numCategories do
		local name, ep = installData:match("^(%Z+)%z()", pos)
		categories[i], cbpos[i], pos = name, ep+2, ep + 2 + bitfieldLength
	end
	
	local files = {}
	for i=1, numFiles do
		local name, ep = installData:match("^(%Z+)%z()", pos)
		local cats, catmap, cbyte, cmod = "", {}, math.floor((i-1)/8), 2^(8 - ((i-1) % 8))
		local ctest = cmod/2
		for j=1, numCategories do
			if byte(installData, cbpos[j]+cbyte) % cmod >= ctest then
				catmap[categories[j]], cats = true, (cats and cats .. ";" or cats) .. categories[j]
			end
		end
		files[i], pos = {
			name=name,
			contentHash=bin.to_hex(installData:sub(ep, ep+15)),
			size=bin.uint32_be(installData, ep+15),
			tags=cats or "",
			tagSet=catmap,
		}, ep + 20
	end
	
	return files, categories
end

function M.files(installData, filter)
	assert(type(installData) == "string", 'Syntax: casc.install.files("installData"[, "filter"])')
	assert(filter == nil or type(filter) == "string", 'Syntax: casc.install.files("installData"[, "filter"])')
		
	local files, ft, i = M.parseInstall(installData), {}, 1
	if filter then
		for f in filter:gmatch("[^;]+") do
			ft[#ft+1] = f
		end
	end
	
	return function()
		local f, m repeat
			i, f, m = i + 1, files[i], true
			if f then
				for j=1, #ft do
					if not f.tagSet[ft[j]] then
						m = false
						break
					end
				end
				if m then
					return f.name, f.contentHash, f.size, f.tags
				end
			end
		until not f
	end
end

return M