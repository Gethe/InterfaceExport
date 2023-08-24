-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local M = {}
local plat = require("casc.platform")


function M.retrieveProductInfo(hostport, product, infoFile)
	if type(hostport) ~= "string" or type(product) ~= "string" or type(infoFile) ~= "string" then
		error('Syntax: ribbit.retrieveProductInfo("host[:port]", "product", "infoFile")', 2)
	end
	if not plat.socketQuery then
		return nil, "ribbit requires socket access"
	end
	
	local host, port = hostport:match("^([^:]+):(%d+)$")
	if not host then
		host, port = hostport, 1119
	end
	local data, err = plat.socketQuery(host, port+0, "v1/products/" .. product .. "/" .. infoFile .. "\r\n")
	if not data then
		return nil, "socket error: " .. tostring(err)
	end
	
	local dkey = data:match([[^Content%-Type: multipart/alternative; boundary="([^"]+)"]])
	if not dkey then
		return nil, "multipart reply expected; found no boundary"
	end
	local segments = {}
	local boundaryText, finalBoundaryText = "\r\n--" .. dkey .. "\r\n", "\r\n--" .. dkey .. "--\r\n"
	local bp, bpE = string.find(data, boundaryText, 1, true)
	repeat
		local np, npE = string.find(data, boundaryText, bpE, true)
		if not np then
			np, npE = string.find(data, finalBoundaryText, bpE, true)
		end
		if np then
			local segment, headers, body = data:sub(bpE+1, np-2)
			if segment:sub(1,2) == "\r\n" then
				headers, body = nil, segment
			else
				headers, body = segment:match("^(.-)\r\n\r\n(.+)$")
			end
			segments[#segments+1] = {body=body, headers=headers}
			bp, bpE = np, npE
		end
	until not np
	-- There may be additioanl headers after the final boundary, but let's just ignore that.

	if segments[1] then
		return segments[1].body
	end
end

return M