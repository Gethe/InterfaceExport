local M, bit = {}, require("casc.platform")
local bxor, band, bor, rol, bnot = bit.bxor, bit.band, bit.bor, bit.rol, bit.bnot
local schar, sbyte, floor = string.char, string.byte, math.floor

local s, K = {[0]=
	7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
	5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
	4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
	6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,
}, {[0]=
	0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
	0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
	0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
	0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
	0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
	0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
	0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
	0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
}

local function int64(n)
	return schar(floor(n) % 256, floor(n / 256) % 256, floor(n / 256^2) % 256, floor(n / 256^3) % 256,
		floor(n / 256^4) % 256, floor(n / 256^5) % 256, floor(n / 256^6) % 256, floor(n / 256^7) % 256)
end

function M.sumhexa(m)
	assert(#m < 2^50, "md5: input too long") -- for double lua_Number; you're on your own elsewhere
	local h0, h1, h2, h3, M, ll = 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, {}, #m % 64
	local pad, cut = m:sub(-ll, ll == 0 and 0 or -1) .. schar(128) .. schar(0):rep((56 - (#m + 1) % 64) % 64) .. int64(#m*8), #m - ll
	for p=1, #m+#pad-ll, 64 do
		if p > cut then
			m, p = pad, p - cut
		end
		for i=0,15 do
			local a, b, c, d = sbyte(m, p+i*4, p+i*4+3)
			M[i] = d * 256^3 + c * 256^2 + b * 256 + a
		end
		local A, B, C, D, F, g = h0, h1, h2, h3
		for i=0, 63 do
			if i <= 15 then
				F, g = bxor(D, band(B, bxor(C, D))), i
			elseif i <= 31 then
				F, g = bxor(C, band(D, bxor(B, C))), (5*i + 1) % 16
			elseif i <= 47 then
				F, g = bxor(bxor(B, C), D), (3*i + 5) % 16
			else
				F, g = bxor(C, bor(B, bnot(D))), (7*i) % 16
			end
			D, C, B, A = C, B, B + rol((A + F + K[i] + M[g]), s[i]), D
		end
		h0, h1, h2, h3 = (h0 + A) % 2^32, (h1 + B) % 2^32, (h2 + C) % 2^32, (h3 + D) % 2^32
	end
	
	return (("%08x%08x%08x%08x"):format(h0, h1, h2, h3):gsub("(..)(..)(..)(..)", "%4%3%2%1"))
end

return M