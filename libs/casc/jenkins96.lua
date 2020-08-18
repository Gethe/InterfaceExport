local M, plat, bin = {}, require("casc.platform"), require("casc.bin")

local rot, xor, uint32_le, to_le32 = plat.rol, plat.bxor, bin.uint32_le, bin.to_le32

function M.hash(k)
	assert(type(k) == "string", 'Syntax: casc.jenkins96.hash("key")')
	
	if #k == 0 then return 0xdeadbeef, 0xdeadbeef end
	local a = 0xdeadbeef + #k
	local b, c, k = a, a, k .. (k ~= "" and ("\0"):rep((12 - #k % 12) % 12) or "")
	for i=0, #k-13, 12 do
		a, b, c = a + uint32_le(k, i), b + uint32_le(k, i+4), c + uint32_le(k, i+8)
		a, c = xor(a-c, rot(c, 4)), c + b
	   b, a = xor(b-a, rot(a, 6)), a + c
	   c, b = xor(c-b, rot(b, 8)), b + a
	   a, c = xor(a-c, rot(c,16)), c + b
	   b, a = xor(b-a, rot(a,19)), a + c
	   c, b = xor(c-b, rot(b, 4)), b + a
	end
	local i = #k - 12
	a, b, c = a + uint32_le(k, i), b + uint32_le(k, i+4), c + uint32_le(k, i+8)
	c = xor(c, b) - rot(b,14)
	a = xor(a, c) - rot(c,11)
	b = xor(b, a) - rot(a,25)
	c = xor(c, b) - rot(b,16)
	a = xor(a, c) - rot(c,04)
	b = xor(b, a) - rot(a,14)
	c = xor(c, b) - rot(b,24)
	return c, b
end

function M.hash_path(path)
	assert(type(path) == "string", 'Syntax: casc.jenkins96.hash_path("path")')
	local c, b = M.hash((path:upper():gsub('/', '\\')))
	return to_le32(b) .. to_le32(c)
end

return M