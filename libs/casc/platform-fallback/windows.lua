-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, P = {}, require("casc.platform")

M.TMP_PATH_PREFIX = os.getenv('TMP') or os.getenv('TEMP') or nil
M.commands = {toDevNull=' 2>NUL', ls='(for %%a in (%s) do @echo %%~fa)', mkdir='mkdir %s', gzip='gzip -dcq %s'}

local function execute(...)
	local ok, status, sig = os.execute(...)
	if ok == true and status == "exit" or status == "signal" then
		return sig
	else
		return ok or sig or ok, status, sig
	end
end
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

function M.decompress(compressed)
	assert(type(compressed) == "string", 'Syntax: casc.platform.decompress("compressed")')
	assert(M.commands and M.commands.gzip and M.commands.toDevNull, 'unsupported platform')

	local f, f2 = P.tmpname(), P.tmpname()
	local h = io.open(f, "wb")
	h:write('\31\139\8\0\0\0\0\0')
	h:write(compressed)
	h:close()

	execute(M.commands.gzip:format(shellEscape(f)) .. " 1>" .. f2 .. " " .. M.commands.toDevNull)
	os.remove(f)

	return readAndDeleteFile(f2)
end
function M.mkdir(path)
	assert(type(path) == 'string', 'Syntax: casc.platform.mkdir("path")')
	assert(M.commands and M.commands.mkdir, 'unsupported platform')
	
	return execute(M.commands.mkdir:format(shellEscape(path)))
end
function M.files(dir, glob)
	assert(type(dir) == "string" and type(glob) == 'string', 'Syntax: casc.platform.files("dir", "glob")')
	assert(M.commands and M.commands.ls, 'unsupported platform')

	local t, ni, h = {}, 1, io.popen(M.commands.ls:format(shellEscape(P.path(dir, "")) .. glob), "r")
	for l in h:lines() do
		t[ni], ni = l, ni + 1
	end
	h:close()
	return pairs(t)
end
function M.http(url, h)
	local c, of = "curl -s -S -A 'luacasc+curl'", P.tmpname()
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


for k,v in pairs(M) do
	if P[k] == nil then
		P[k], P._IMPL[k] = v, "platform-fallback/windows"
	end
end

return M