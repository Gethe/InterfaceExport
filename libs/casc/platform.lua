-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, assert = {_IMPL={}}, assert

local function maybe(m)
	local ok, v = pcall(require, m)
	return ok and v
end
local lfs = maybe("lfs") -- LuaFileSystem; http://keplerproject.github.io/luafilesystem/
local zlib = maybe("zlib") -- lzlib; https://github.com/LuaDist/lzlib
local bit = maybe("bit") -- Lua BitOp; http://bitop.luajit.org
local socket = maybe("socket.http") -- LuaSocket; http://w3.impa.br/~diego/software/luasocket/home.html
local curl = not socket and maybe("luacurl") -- LuaCURL; -- http://luacurl.luaforge.net
local md5 = maybe("md5") -- MD5; http://keplerproject.org/md5/

local function shellEscape(s)
	return '"' .. s:gsub('"', '\\"') .. '"'
end
local function readAndDeleteFile(path)
	local h, err = io.open(path, "rb")
	if h then
		local c = h:read("*a")
		h:close()
		h, err = c, nil
	end
	os.remove(path)
	return h, err
end
local function execute(...)
	local ok, status, sig = os.execute(...)
	if ok == true and status == "exit" or status == "signal" then
		return sig
	else
		return ok or sig or ok, status, sig
	end
end

local dir_sep = package and package.config and package.config:sub(1,1) or "/"
M.commands =
	dir_sep == '/' and {toDevNull=' 2>/dev/null', ls='ls %s', mkdir='mkdir -p %s', gzip='gzip -dcq %s'} or
	dir_sep == '\\' and {toDevNull=' 2>NUL', ls='(for %%a in (%s) do @echo %%~fa)', mkdir='mkdir %s', gzip='gzip -dcq %s', TMP=os.getenv('TMP') or os.getenv('TEMP')}

do -- M.path(a, b, ...)
	local dir_sep_pq = dir_sep:gsub("[%[%].%-+*?()%%]", "%%%0")
	M.path = function(a, b, ...)
		if a and b then
			while b == ".." do
				local p, c = a:match("^(.-)([^" .. dir_sep_pq .. "]*)" .. dir_sep_pq .. "?$")
				if c == ".." then
					break
				elseif p == "" and c:match(dir_sep == "\\" and "^[A-Za-z]:$" or "^$") then
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
end
M.url = function(a, b, ...)
	if a and b then
		return M.url(a .. ((a:sub(-1) == "/" or b:sub(1,1) == "/") and "" or "/") .. b, ...)
	end
	return a
end

M.tmpname = function()
	local tn = os.tmpname()
	return (M.commands and M.commands.TMP or "") .. tn
end

M.decompress = zlib and zlib.decompress or function(compressed)
	assert(type(compressed) == "string", 'Syntax: casc.platform.decompress("compressed")')
	assert(M.commands and M.commands.gzip and M.commands.toDevNull, 'unsupported platform')
	
	local f, f2 = M.tmpname(), M.tmpname()
	local h = io.open(f, "wb")
	h:write('\31\139\8\0\0\0\0\0')
	h:write(compressed)
	h:close()
	
	execute(M.commands.gzip:format(shellEscape(f)) .. " 1>" .. f2 .. " " .. M.commands.toDevNull)
	os.remove(f)
	
	return readAndDeleteFile(f2)
end
M._IMPL.decompress = zlib and zlib.decompress and "lzlib" or M.commands and M.commands.gzip and "external executable"

M.mkdir = lfs and lfs.mkdir or function(path)
	assert(type(path) == 'string', 'Syntax: casc.platform.mkdir("path")')
	assert(M.commands and M.commands.mkdir, 'unsupported platform')
	
	return execute(M.commands.mkdir:format(shellEscape(path)))
end
M._IMPL.mkdir = lfs and lfs.mkdir and "LuaFileSystem" or M.commands and M.commands.mkdir and "external executable"

M.files = lfs and lfs.dir and function(dir, glob)
	assert(type(dir) == "string" and type(glob) == 'string', 'Syntax: casc.platform.files("dir", "glob")')
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
end or function(dir, glob)
	assert(type(dir) == "string" and type(glob) == 'string', 'Syntax: casc.platform.files("dir", "glob")')
	assert(M.commands and M.commands.ls, 'unsupported platform')
	
	local t, ni, h = {}, 1, io.popen(M.commands.ls:format(shellEscape(M.path(dir, "")) .. glob), "r")
	for l in h:lines() do
		t[ni], ni = l, ni + 1
	end
	h:close()
	return pairs(t)
end
M._IMPL.files = lfs and lfs.dir and "LuaFileSystem" or M.commands and M.commands.ls and "external executable"

local function checkBitModule(bit, name)
	if bit and bit.bnot and bit.bxor and bit.band and bit.bor then
		M.rol, M.bnot, M.bxor, M.band, M.bor = bit.rol or bit.lrotate, bit.bnot, bit.bxor, bit.band, bit.bor
		M._IMPL.bit = name
		return true
	end
end
if checkBitModule(bit, "bit module") or checkBitModule(bit32, "bit32 global") then
else
	M._IMPL.bit = "CASC shim"
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

if socket then
	socket.USERAGENT, socket.TIMEOUT = "luacasc", 5
	local ltn12, RETRIES = require("ltn12"), 3
	M.http = function(url, h)
		for i=1,RETRIES do
			local sink = {}
			local ok, status, head = socket.request({url=url, sink=ltn12.sink.table(sink), headers=h})
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
	M._IMPL.http = "LuaSocket module"
	
	local rawSocket = require("socket")
	if rawSocket then
		M.socketQuery = function(host, port, query)
			local client, err, ok = rawSocket.connect(host, port)
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
		M._IMPL.socketQuery = "LuaSocket module"
	end
elseif curl then
	local function writeSink(sink, buf)
		sink[#sink+1] = buf
		return #buf
	end
	local function sink(c)
		local self = {}
		c:setopt(curl.OPT_WRITEFUNCTION, writeSink)
		c:setopt(curl.OPT_WRITEDATA, self)
		return self
	end
	M.http = function(url, h)
		local c = curl.new()
		c:setopt(curl.OPT_URL, url)
		c:setopt(curl.OPT_USERAGENT, "luacasc")
		if h and h.Range then
			c:setopt(curl.OPT_RANGE, h.Range:match("[%d%-]+"))
		end
		local s = sink(c)
		c:perform()
		local status = c:getinfo(curl.INFO_RESPONSE_CODE)
		if (status or 0) < 200 or status >= 300 then
			local err = "http request failed: " .. url .. "; http " .. tostring(status) .. "/" .. tostring(c:getinfo(curl.INFO_OS_ERRNO))
			c:close()
			return nil, err, status
		else
			c:close()
			return table.concat(s, "")
		end
	end
	M.socketQuery = function(host, port, query)
		local c, o = curl.new(), {}
		c:setopt(curl.OPT_WRITEDATA, o)
		c:setopt(curl.OPT_READDATA, {query})
		c:setopt(curl.OPT_WRITEFUNCTION, function(sink, buf)
			sink[#sink+1] = buf
		end)
		c:setopt(curl.OPT_READFUNCTION, function(b, nb)
			local s, rest = b[1] or ""
			if s and #s > nb then
				s, rest = s:sub(1,nb-1), s:sub(nb)
			end
			b[1] = rest
			return s
		end)
		c:setopt(curl.OPT_URL, "telnet://" .. host .. ":" .. port)
		c:perform()
		if not o[1] then
			return nil, "no data in response"
		end
		return table.concat(o, "")
	end
	M._IMPL.http = "LuaCURL module"
	M._IMPL.socketQuery = "LuaCURL module"
else
	M.http = function(url, h)
		local c, of = "curl -s -S -A 'luacasc+curl'", M.tmpname()
		if type(h) == "table" then
			for k,v in pairs(h) do
				c = c .. ' -H ' .. shellEscape(k .. ": " .. v)
			end
		end
		c = c .. ' -o ' .. shellEscape(of) .. ' ' .. shellEscape(url)
		local ret = execute(c)
		if ret == 0 then
			return readAndDeleteFile(of)
		end
		os.remove(of)
		return nil, "HTTP request failed; status " .. tostring(ret)
	end
	M._IMPL.http = "external executable"
end

M.md5, M._IMPL.md5 = md5 and md5.sumhexa, "MD5"
if not M.md5 then
	package.loaded["casc.platform"] = M
	local lmd5 = maybe("casc.md5")
	M.md5 = lmd5 and lmd5.sumhexa or function()
		error("No MD5 implementation available")
	end
	M._IMPL.md5 = lmd5 and lmd5.sumhexa and "CASC.MD5"
end

return M