-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M = {_IMPL={}}

local function maybe(m)
	local ok, v = pcall(require, m)
	return ok and v
end

local lfs = maybe("lfs") -- LuaFileSystem; https://github.com/lunarmodules/luafilesystem
local zlib = maybe("zlib") -- lzlib; https://github.com/LuaDist/lzlib
local bit = maybe("bit") -- Lua BitOp; https://bitop.luajit.org
local socket = maybe("socket.http") -- LuaSocket; https://github.com/lunarmodules/luasocket
local socket_root, socket_ltn12 = socket and maybe("socket"), socket and maybe("ltn12")
local curl = not (socket_root and socket_ltn12) and maybe("luacurl") -- LuaCURL; -- https://luarocks.org/modules/luarocks/luacurl
local md5 = maybe("md5") -- MD5; https://github.com/lunarmodules/md5

local dir_sep = package and package.config and package.config:sub(1,1) or "/"
local dir_sep_pq = dir_sep:gsub("[%[%].%-+*?()%%]", "%%%0")

function M.path(a, b, ...)
	if a and b then
		local noUT = M.NO_PARENT_PATH_PATTERN
		noUT = noUT == nil and (dir_sep == "\\" and "^[A-Za-z]:$" or "^$") or noUT
		while b == ".." do
			local p, c = a:match("^(.-)([^" .. dir_sep_pq .. "]*)" .. dir_sep_pq .. "?$")
			if c == ".." then
				break
			elseif p == "" and noUT and c:match(noUT) then
				error("bad path traversal")
			elseif p == "" then
				p = c == "." and ".." or "."
			end
			return M.path(p, ...)
		end
		return M.path(a .. (a:sub(-1) ~= dir_sep and dir_sep or "") .. b, ...)
	end
	return a
end
function M.url(a, b, ...)
	if a and b then
		return M.url(a .. ((a:sub(-1) == "/" or b:sub(1,1) == "/") and "" or "/") .. b, ...)
	end
	return a
end
function M.tmpname()
	local tn = os.tmpname()
	return (M.TMP_PATH_PREFIX or "") .. tn
end

if zlib and zlib.decompress then
	M.decompress, M._IMPL.decompress = zlib.decompress, "lzlib"
end
if lfs and lfs.mkdir then
	M.mkdir, M._IMPL.mkdir = lfs.mkdir, "LuaFileSystem"
end
if lfs and lfs.dir then
	function M.files(dir, glob)
		if not (type(dir) == "string" and type(glob) == 'string') then
			return error('Syntax: casc.platform.files("dir", "glob")', 2)
		end
		local pat = "^" .. glob:gsub("%.%-%+", "%%%0"):gsub("%*", ".*") .. "$"
		local t, ni = {}, 1
		local ok, it, is, ik = pcall(lfs.dir, dir)
		if ok then
			for f in it, is, ik do
				if f ~= "." and f ~= ".." and f:match(pat) then
					t[ni], ni = M.path(dir, f), ni + 1
				end
			end
		end
		return pairs(t)
	end
	M._IMPL.files = "LuaFileSystem"
end

local function checkBitModule(bit, name)
	if bit and bit.bnot and bit.bxor and bit.band and bit.bor then
		M.rol, M.bnot, M.bxor, M.band, M.bor = bit.rol or bit.lrotate, bit.bnot, bit.bxor, bit.band, bit.bor
		M._IMPL.bit = name
		return true
	end
end
if not (checkBitModule(bit, "bit module") or checkBitModule(bit32, "bit32 global")) then
	M._IMPL.bit = "LuaCASC"
	local MAX_INT32, bxorT, bxorW = 2^32 - 1, {[0]={[0]=0, 1}, {[0]=1, 0}}, 2
	function M.bnot(a)
		return MAX_INT32 - a
	end
	local function bxor(a, b)
	  local res, c, a, b = 0, 1, a % 2^32, b % 2^32
	  while a > 0 and b > 0 do
	    local a2, b2 = a % bxorW, b % bxorW
	    res = res + bxorT[a2][b2]*c
	    a, b, c = (a - a2) / bxorW, (b - b2) / bxorW, c * bxorW
	  end
	  return res + (a + b) * c
	end
	local function band(a, b)
		a, b = a % 2^32, b % 2^32
		return (a + b - bxor(a, b)) / 2
	end
	local function bor(a, b)
		return MAX_INT32 - band(MAX_INT32 - a, MAX_INT32 - b)
	end
	M.bxor, M.band, M.bor = bxor, band, bor
	for k=1,3 do
		for i=0, 2^2^k-1 do
			local ti = bxorT[i] or {}
			for j=0, 2^2^k-1 do
				ti[j] = ti[j] or (j < i and bxorT[j][i]) or bxor(i, j)
			end
			bxorT[i] = ti
		end
		bxorW = 2^2^k
	end
end
M.rol = M.rol or function(n, b)
	local n, e2 = n % 2^32, 2^(32-b)
	local lo = n % e2
	return lo * 2^b + (n - lo)/e2
end

if socket and socket.request and socket_ltn12 then
	socket.USERAGENT, socket.TIMEOUT = "luacasc", 5
	local RETRIES = 3
	M.http = function(url, h)
		for i=1,RETRIES do
			local sink = {}
			local ok, status, head = socket.request({url=url, sink=socket_ltn12.sink.table(sink), headers=h})
			if ok then
				local cnt = table.concat(sink, "")
				if type(status) ~= "number" or status < 200 or status >= 300 then
					return nil, "HTTP request failed: " .. tostring(status) .. "; URL: " .. tostring(url), status, head, cnt
				end
				return status >= 200 and status < 300 and cnt or nil, status, head, cnt
			elseif i == RETRIES then
				return nil, "HTTP request failed: " .. tostring(status) .. "; URL: " .. tostring(url), status
			end
		end
	end
	M._IMPL.http = "LuaSocket"
end
if socket_root and socket_root.connect then
	M.socketQuery = function(host, port, query)
		local client, err, ok = socket_root.connect(host, port)
		if not client then
			return nil, "on connect: " .. string(err)
		end
		client:settimeout(0, "t")
		ok, err = client:send(query)
		if not ok then
			return nil, "on send: " .. tostring(err)
		end
		client:settimeout(nil, "t")
		ok, err = client:receive("*a", "")
		client:close()
		return ok, err
	end
	M._IMPL.socketQuery = "LuaSocket"
end

if curl and not (M.http and M.socketQuery) then
	local function writeSink(sink, buf)
		sink[#sink+1] = buf
		return #buf
	end
	local function readSource(source, nb)
		local s, rest = source[1] or ""
		if s and #s > nb then
			s, rest = s:sub(1,nb-1), s:sub(nb)
		end
		source[1] = rest
		return s
	end
	if not M.http then
		function M.http(url, h)
			local c, o = curl.new(), {}
			c:setopt(curl.OPT_URL, url)
			c:setopt(curl.OPT_USERAGENT, "luacasc")
			c:setopt(curl.OPT_WRITEDATA, o)
			c:setopt(curl.OPT_WRITEFUNCTION, writeSink)
			if h and h.Range then
				c:setopt(curl.OPT_RANGE, h.Range:match("[%d%-]+"))
			end
			c:perform()
			local status, eno = c:getinfo(curl.INFO_RESPONSE_CODE), c:getinfo(curl.INFO_OS_ERRNO)
			c:close()
			if (status or 0) < 200 or status >= 300 then
				local err = "http request failed: " .. url .. "; http " .. tostring(status) .. "/" .. tostring(eno)
				return nil, err, status
			else
				return table.concat(o, "")
			end
		end
		M._IMPL.http = "LuaCURL"
	end
	if not M.socketQuery then
		function M.socketQuery(host, port, query)
			local c, o = curl.new(), {}
			c:setopt(curl.OPT_URL, "telnet://" .. host .. ":" .. port)
			c:setopt(curl.OPT_WRITEDATA, o)
			c:setopt(curl.OPT_READDATA, {query})
			c:setopt(curl.OPT_WRITEFUNCTION, writeSink)
			c:setopt(curl.OPT_READFUNCTION, readSource)
			c:perform()
			c:close()
			if not o[1] then
				return nil, "no data in response"
			end
			return table.concat(o, "")
		end
		M._IMPL.socketQuery = "LuaCURL"
	end
end

if md5 and md5.sumhexa then
	M.md5, M._IMPL.md5 = md5.sumhexa, "MD5"
else
	package.loaded["casc.platform"] = M
	local lmd5 = maybe("casc.md5")
	M.md5 = lmd5 and lmd5.sumhexa or nil
	M._IMPL.md5 = lmd5 and lmd5.sumhexa and "LuaCASC" or nil
end

return M