-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/dbc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = {MAX_GUESS_ATTEMPTS=50, TARGET_TEST_SET_SIZE=100, PREFER_ERRORS=true, _VERSION="LuaDBC 1.13"}, require("dbc.bin")

local uint32_le, int32_le = bin.uint32_le, bin.int32_le
local float32_le, uint16_le = bin.float32_le, bin.uint16_le
local int_le, uint_le = bin.int_le, bin.uint_le
local pint_le, upint_le, u32_float = bin.pint_le, bin.upint_le, bin.u32_float

local POSSIBLE_FLOAT_PACK_TYPES = {[1]=true, [3]=true, [4]=true}

local function assertLEQ(a, b, message)
	if a > b then
		error(message .. ": " .. a .. " > " .. b, 2)
	end
end
local function setFlags(f, ...)
	for i=1,select("#", ...) do
		local v = select(i, ...)
		if v then
			f[v] = true
		end
	end
end
local function serializeFlags(f)
	local fa = {}
	for k, v in pairs(f) do
		if v then
			fa[#fa+1] = k
		end
	end
	table.sort(fa)
	return table.concat(fa, " ")
end

local function guessInlineStrings(s, fields, fi, fmt, pos)
	local stringState = {}
	local p = 0
	for i=1,fields do
		local fc = fmt:sub(i,i)
		if fc == "s" and fi[i][1] ~= 4 then
			error('Invalid signature: declared string field has unexpected width', 3)
		end
		if fi[i][1] == 4 and (fc == "?" or fc == "s" or fc == "") then
			stringState[i] = "?"
		else
			stringState[i] = "."
		end
		p = p + fi[i][1]
	end

	local traceArchive = {}
	for i=1,#pos do
		local base, oEnd, currentTrace = pos[i], pos[-i]-pos[i], {[0]={[-1]={nil,fi[1][1]}, [0]={{},{}}}}
		local function insertQueueEntry(fid, ofs, src, k)
			if ofs > oEnd then return end
			local builtNode = src do
				local srcNode = currentTrace[src][fid-1]
				builtNode = {src}
				if fid == 1 then
					for u=2,i do
						local otherRootFW = traceArchive[u-1][0][-1]
						builtNode[u] = {[otherRootFW[k]] = 1}
					end
				else
					local fsz = fi[fid][1]
					for u=2,i do
						for k2=1,2 do
							local srcArr = srcNode[k2]
							for i=1,#srcArr do
								local otherTrace, otherSrc = traceArchive[u-1], srcArr[i][u]
								for j in pairs(otherSrc) do
									local otherSrc = otherTrace[j]
									local otherFW = otherSrc and otherSrc[-1]
									local otherDst = k == 2 and (j+fsz) or (otherFW and otherFW[k])
									local otherDstNode = otherTrace[otherDst]
									if otherDstNode and otherDstNode[fid] and otherDstNode[fid][k] then
										local as = builtNode[u] or {}
										as[otherDst] = 1
										builtNode[u] = as
									end
								end
							end
						end
						if not builtNode[u] then
							return
						end
					end
				end
			end
			
			local e = currentTrace[ofs]
			if not e then
				e = {}
				currentTrace[ofs] = e
			end
			
			if k == 1 then
				currentTrace[src][-1][k] = ofs
			end
			
			e[fid] = e[fid] or {{},{}}
			e = e[fid][k]
			e[#e+1] = builtNode
		end
		
		for ofs=0,oEnd do
			if currentTrace[ofs] then
				currentTrace[ofs][-1] = currentTrace[ofs][-1] or {}
				for ofid in pairs(currentTrace[ofs]) do
					local fid = ofid+1
					if fid > 0 and fid <= fields then
						local state = stringState[fid]
						if state ~= "." then
							local sr, nofs = s:match("(%Z*)()", 1+ofs+base)
							if not sr:match("[^%C\n\r\t]") then
								insertQueueEntry(fid, nofs-base, ofs, 1)
							end
						end
						if state ~= "s" then
							insertQueueEntry(fid, ofs + fi[fid][1], ofs, 2)
						end
					end
				end
			end
		end

		-- Remove parallel traces
		for _k, v in pairs(currentTrace) do
			for f, m in pairs(v) do
				for i=1,f == -1 and 0 or 2 do
					local a = m[i]
					for j=1,#a do
						a[j] = a[j][1]
					end
				end
			end
		end
		
		-- Mark anything that reaches into the null tail of the record as "needed"
		local nullEnd = #s:sub(1+pos[i], pos[-i]):match("(%z*)$")
		currentTrace[oEnd] = currentTrace[oEnd] or {}
		currentTrace[oEnd][fields] = currentTrace[oEnd][fields] or {{},{}}
		for i=oEnd-nullEnd,oEnd do
			local e = currentTrace[i]
			if e and e[fields] then
				e.n = e.n or {[fields]=1}
			end
		end
		
		-- Cull links which cannot lead to the null tail
		local seenFieldState = {}
		for i=oEnd,1,-1 do
			local e = currentTrace[i]
			if e == nil or not e.n then
				currentTrace[i] = nil
			else
				local n = e.n
				for f, a in pairs(e) do
					if f == -1 then
					elseif not n[f] then
						e[f] = nil
					else
						for k=1,2 do
							local a = a[k]
							seenFieldState[#a == 0 and 0 or k == 1 and -f or f] = true
							for j=1,#a do
								local e2 = currentTrace[a[j]]
								local nt = e2.n or {}
								nt[f-1], e2.n = 1, nt
							end
						end
					end
				end
			end
		end
		traceArchive[i] = currentTrace
		
		-- Some fields may only appear as strings or non-strings on goal-reaching traces;
		-- this can restrict future traces, and if no choices remain, return an early end.
		local fixedStringState = true
		for i=1,fields do
			if stringState[i] == "?" then
				local seenFixed, seenString = seenFieldState[i], seenFieldState[-i]
				if seenFixed and not seenString then
					stringState[i] = "."
				elseif seenString and not seenFixed then
					stringState[i] = "s"
				else
					fixedStringState = false
				end
			end
		end
		if fixedStringState then
			return table.concat(stringState, "", 1, fields)
		end
	end

	-- There are multiple options remaining.
	local validMasks, outputMaskBuffer = {}, {}
	local function backSearch(front, fid)
		if fid == 0 then
			validMasks[#validMasks+1] = table.concat(outputMaskBuffer, "", 1, fields)
			return
		end
		local az = fid == 1 and 0 or "NOT A VALID FOLLOW INDEX"
		local nfid, oc, f1, f2 = fid-1, 0
		for k=2,1,-1 do
			local nf = {}
			for i=1,#front do
				local nfi = {}
				for ii=1,#front[i] do
					local a = front[i][ii][k]
					for j=1,#a do
						local np = a[j]
						nfi[#nfi+1] = np == az or traceArchive[i][np][nfid]
					end
				end
				if not nfi[1] then
					f1, f2, nf = k == 1 and i or f1, k == 2 and i or f2
					break
				end
				nf[i] = nfi
			end
			if nf then
				outputMaskBuffer[fid] = k == 1 and "s" or "."
				backSearch(nf, nfid)
				oc = oc + 1
			end
		end
	end
	local frontier = {} -- Merge every offset claiming to have a solution for [fields]
	for i=1,#traceArchive do
		local m1, m2 = {}, {}
		for _k, v in pairs(traceArchive[i]) do
			if v[fields] then
				for k=1,2 do
					local a,m = v[fields][k], k == 1 and m1 or m2
					for i=1,#a do
						m[a[i]] = 1
					end
				end
			end
		end
		local a1, a2 = {}, {}
		for k in pairs(m1) do a1[#a1+1] = k end
		for k in pairs(m2) do a2[#a2+1] = k end
		frontier[i] = {{a1, a2}}
	end
	backSearch(frontier, fields)
	
	return validMasks[1], validMasks
end
local function extendType(count, sym)
	return sym:rep(tonumber(count))
end
local function updateFloatScores(ic, fc, iv, fv)
	fv = fv < 0 and -fv or fv
	return ic+1, fc + (fv >= 1e-7 and fv <= 1e16 and 1 or fv == 0 and 0.75 or -1)
end
local function guessTypes(s, header, fmt)
	local rows, fields = header.rows, header.fields
	if rows < 1 then return ("u"):rep(fields) end
	
	local sbase, fi, is = header.stringBase, header.fieldInfo, header.inlineStrings
	local redo, pos, slen = 0, {}, header.stringLength or 0
	local rowRelativeStrings = header.rowRelativeStrings
	local stringShift = header.stringShift or 0
	local stringBlockMap = header.mapStringOffset
	local rpos = {} do
		local t, dropPad = {}, header.dropPadding or 0
		for i=-5,4 do t[i % rows] = 1 end
		local sm = math.max(1,math.ceil((rows-15)/M.TARGET_TEST_SET_SIZE))
		for i=10, rows-5,sm do t[i] = 1 end
		local rl, base, stride = header.rowList, header.rowBase, header.stride
		for k in pairs(t) do
			pos[#pos+1] = rl and rl[k+1][2] or (base + k*stride)
			pos[-#pos], rpos[#pos] = pos[#pos]+(rl and rl[k+1][3] or stride)-dropPad, pos[#pos]
		end
	end
	
	fmt = fmt and fmt:gsub("[{}]", ""):gsub("[^uifsFL%d*]", "?"):gsub("(%d+)(%D)", extendType) or "*?"
	fmt = fmt:gsub("%*(.)", ("%1"):rep(fields-#fmt+1))
	local userFormat, o = fmt
	
	local isFormatHint = fi and is and guessInlineStrings(s, fields, fi, fmt, pos)
	repeat
		fmt, o = fmt .. ("?"):rep(fields-#fmt), ""
		for i=1,redo > 0 and #pos or 0 do
			pos[i] = rpos[i]
		end
		
		local nsc, maybeStrings = 0, is and {}
		for j=1,fields do
			local fj = fi and fi[j]
			local fsz, ft = fj and fj[1] or 4, fmt:sub(j,j)
			if ft ~= "?" then
			elseif fj and fj.packType == "ForeignKeyMap" then
				ft = "F"
			elseif fsz == 0 then
				ft = fj and fj.packType == 1 and fj.pa3 and fj.pa3 % 2 == 1 and "i" or "u"
				local ic, fc = 0, 0
				for i=1, ft == "u" and POSSIBLE_FLOAT_PACK_TYPES[fj.packType] and #pos or 0 do
					local iv
					if fj.packType == 1 then
						iv = upint_le(s, fj.bitSize, rpos[i]*8+fj.bitOffset)
					elseif fj.packType == 3 or fj.packType == 4 then
						local cf = j
						local extraSkip, aw = fj.packType == 4 and 4*(cf-fj.firstFieldIndex) or 0, fj.packType == 4 and 4*fj.pa3 or 4
						local ix = upint_le(s, fj.bitSize, rpos[i]*8+fj.bitOffset)
						iv = uint_le(s, 4, fj.adOfs+extraSkip+ aw*ix)
					end
					if iv then
						ic, fc = updateFloatScores(ic, fc, iv, u32_float(iv))
					end
				end
				if fc > 0 and fc >= ic/1.25 then
					ft = "f"
				end
			elseif fsz == 4 then
				local sc, ic, fc, nb = 0, 0, 0, 0
				for i=1,#pos do
					local uv, fv = uint32_le(s, pos[i]), float32_le(s, pos[i])
					nb = nb + (is and uv % 256 == 0 and 1 or 0)
					if uv ~= 0 then
						local sbase = rowRelativeStrings and 1+pos[i]+stringShift or sbase
						local spos = sbase and (sbase + uv - 1)
						if stringBlockMap then spos = stringBlockMap(spos) end
						local cs = is and s:match("^(%Z*)", 1+pos[i]) or (not is and uv < slen and spos and s:match("^%z(%Z+)", spos))
						if cs and (not is or (cs:match("^[%C\n\r\t]*$") and cs:match("%w%w%w"))) then
							sc, ic = sc + 1, ic - 1
						elseif is and cs == "" then
							sc = sc + math.random(0, 1)
						else
							sc, ic, fc = sc - (is and uv % 256 == 0 and 0 or 1), updateFloatScores(ic, fc, uv, fv)
						end
					end
				end
				if is and isFormatHint then
					sc = ic + (isFormatHint:sub(j,j) == "s" and 1 or -1)
				elseif is and (sc >= ic and redo > 1 and math.random() < 0.10) then
					sc, maybeStrings[#maybeStrings+1] = ic-1, j
				elseif nb == #pos and sc <= ic then
					maybeStrings[#maybeStrings+1] = j
				end
				ft = sc > ic and "s" or (fc > 0 and fc >= ic/1.25 and "f" or "i")
			elseif fsz == 8 and bin.PRECISION_BYTES < fsz then
				ft = "L"
			else
				ft = "u"
			end
			if ft == "s" and is then
				nsc = nsc + (userFormat:sub(j,j) == "s" and 0 or 1)
				for i=1,#pos do
					pos[i] = s:match("%Z*()", 1+pos[i])
				end
			else
				for i=1,#pos do
					pos[i] = pos[i]+fsz
				end
			end
			o = o .. ft
		end
		
		for i=1,is and not isFormatHint and #pos or 0 do
			local cur, exp = pos[i], pos[-i]
			if cur > exp or (exp-cur) > 3 then
				redo = redo + 1
				assert(redo < M.MAX_GUESS_ATTEMPTS, "Exceeded attempt limit; cannot guess row format")
				assert(maybeStrings[1] or nsc > 0, "Irrecoverable format guess")
				if math.random() > 0.5 and maybeStrings[1] then
					fmt, o = o:sub(1, maybeStrings[math.random(#maybeStrings)]-1) .. "s"
				else
					local o2, c = "", math.random(nsc)-1
					for block, sp in o:gmatch("([^s]*)()s?") do
						if userFormat:sub(sp, sp) == "s" then
							o2 = o2 .. block .. "s"
						else
							o2, c = o2 .. block .. (c > 0 and "s" or ""), c-1
							if c < 0 then
								break
							end
						end
					end
					fmt, o = o2
				end
				break
			end
		end
	until o
	
	if header.coTypes and #header.coTypes > 0 then
		local ucTypes, coTypes = userFormat:sub(fields+1), header.coTypes
		for i=1,#header.coTypes do
			local ut = ucTypes:sub(i,i)
			o = o .. ((ut == "?" or ut == "") and coTypes:sub(i,i) or ut)
		end
	end
	return o
end

local function emptyNext()
end
local function createUnpacker(data, header, format, loose, partID, guessedTypes)
	if header.parts ~= nil and partID == nil then
		local largePartRows, largePartID, numPresentParts = 0, 0, 0
		for i=1,#header.parts do
			local p = header.parts[i]
			if p.isPresent and p.rows > 0 then
				numPresentParts = numPresentParts + 1
				if largePartRows < p.rows then
					largePartRows, largePartID = p.rows, i
				end
			end
		end
		if numPresentParts == 0 then
			return emptyNext
		elseif numPresentParts == 1 then
			return createUnpacker(data, header, format, loose, largePartID)
		end
		local guessedTypes do
			local largestPart = header.parts[largePartID]
			local format2 = loose and largestPart.idField and not format:match("%*") and format .. "*." or format
			if format2:match(largestPart.inlineStrings and '[.?]' or '[?]') then
				guessedTypes = guessTypes(data, largestPart, format2)
			end
		 end
		local iterators = {}
		for i=1,#header.parts do
			local p = header.parts[i]
			if p.isPresent and p.rows > 0 then
				iterators[#iterators+1] = createUnpacker(data, header, format, loose, i, guessedTypes)
			end
		end
		local cf, cn, nextRow = iterators[1], 2
		local function checkOutput(s, ...)
			if select("#", ...) > 0 or #iterators < cn then
				return ...
			end
			cf, cn = iterators[cn], cn + 1
			return nextRow(s)
		end
		function nextRow(s)
			return checkOutput(s, cf(s))
		end
		return nextRow, data
	elseif partID and header.parts then
		header = header.parts[partID]
	end
	
	if header.rows == 0 then
		-- This does not validate the signature, but some formats contain conflicting nonsense when empty.
		return emptyNext
	end
	
	local rows, fields, stride, sbase, rbase = header.rows, header.fields, header.stride, header.stringBase, header.rowBase
	local fi, cf, rl, idf, copy = header.fieldInfo, 1, header.rowList, header.idField
	if header.cloneOffset and header.cloneLength > 0 then
		copy = {}
		for i=header.cloneOffset, header.cloneOffset+header.cloneLength-1, 8 do
			local cid, oid = uint32_le(data, i), uint32_le(data, i+4)
			copy[oid], copy[cid] = cid, copy[oid]
		end
	end
	if loose and idf and not format:match("%*") then
		format = format .. "*."
	end
	
	local ctPrefix = copy and '\t' or ''
	local p, po = {"lid"}, {[=[-- casc.dbc:iterator
local smatch, uint_le, int_le, float32_le, upint_le, pint_le, u32_float, cov, idMap, ct, rows, stride, sbase, r0, rList, minId, dfo, fkMap, sbm = ...
local i, ii, lid = 0, nil, nil
return function(data)]=],
	copy and '\tlid = ct[lid]\n\tif rows > i or lid then\n\t\tif not lid then' or '\tif rows > i then',
	ctPrefix..(rl and '\t\tr0, i, lid, ii = rList[i+1][2], i + 1, rList[i+1][1], rList[i+1][1]' or '\t\tr0, i, lid = r0 + stride, i + 1, ' .. (header.idMap and "idMap[i+1]" or "minId+i")),
	copy and '\t\tend' or nil,
	}
	
	local nfi = fi and fi[1]
	local skip, fsz, openTables, cr, crn = nfi and nfi[2] or 0, nfi and nfi[1] or 4, 0, 'r0+', 1
	local gt = format:match(header.inlineStrings and '[.?]' or '[?]') and (guessedTypes or guessTypes(data, header, format))
	local tFields = (header.coVals and #header.coVals or 0) + fields
	local defaultOverrides = {}
	local missingLocalRowIndex = true
	local fkField, fkMap = header.fkField
	local rowRelativeStrings = header.rowRelativeStrings
	local stringShift = header.stringShift or 0
	local stringBlockMap = header.mapStringOffset
	
	for r, t, tp in format:gmatch("(%*?%d*)(.)()") do
		for i=1,r == '*' and (tFields-cf+1) or tonumber(r) or 1 do
			local t, adv = t == '?' and gt:sub(cf, cf) or t
			if cf == idf and fsz <= 5 and missingLocalRowIndex then
				if fsz == 0 and nfi and nfi.bitSize and nfi.bitOffset then
					local se = nfi.packType == 1 and (nfi.pa3 and nfi.pa3 % 2 == 1)
					po[#po+1] = ('%s\t\tlid = %s(data, %d, r0*8+%d)'):format(ctPrefix, se and "pint_le" or "upint_le", nfi.bitSize, nfi.bitOffset), 1
				else
					po[#po+1] = ctPrefix .. '\t\tlid = uint_le(data, ' .. fsz .. ', ' .. cr .. skip .. ')'
				end
				missingLocalRowIndex = false
			end
			if t == '{' then
				p[#p+1], openTables = '{', openTables + 1
				if tp == 2 and #p == 2 then
					p[1], p[2] = '{', '[0]=lid'
				end
			elseif t == '}' then
				assert(openTables > 0, 'invalid signature: no table to close here')
				for j=#p-1, 1, -1 do
					if p[j] == '{' then
						p[j], openTables = '{' .. table.concat(p, ', ', j+1) .. '}', openTables - 1
						for k=#p,j+1,-1 do p[k] = nil end
						break
					end
				end
			elseif i > tFields then
				error("invalid signature: too many specified fields")
			elseif cf > fields and header.coTypes then
				local oid = cf-fields
				local hfType = header.coTypes:sub(oid, oid)
				adv, fsz = 1, 0
				if t == hfType or t == '?' then
					p[#p+1] = ('cov[%d][lid]'):format(oid)
				elseif t ~= '.' then
					error(("invalid signature: extra field type mismatch: %q expected, got %q"):format(hfType, t))
				end
			elseif t == '.' then
				adv = 1
				if header.inlineStrings and gt:sub(cf,cf) == 's' and tp <= #format then
					po[#po+1], cr, crn = 2, ('\t\tlocal r%d = smatch(data, "%%Z*()", 1+%s%d)'):format(crn, cr, skip), 'r'..crn..'+', crn + 1
					skip, fsz = 0, 0
				end
			elseif nfi and nfi.packType == 2 then
				assert((nfi.adOfs and nfi.adLength) or nfi.adLength == 0, 'Default/Override field missing override data')
				if not defaultOverrides[cf] then
					local dv, ofs = nfi.pa1, nfi.adOfs or 0
					local ot = setmetatable({}, {__index=function() return dv end})
					local vr = t == 'i' and int32_le or t == 'f' and float32_le or uint32_le
					for p=ofs,ofs+nfi.adLength-1,8 do
						local rid, v = uint32_le(data, p), vr(data, p+4)
						ot[rid] = v
					end
					defaultOverrides[cf] = ot
				end
				p[#p+1], adv = ('dfo[%d][lid or i]'):format(cf), 1
			elseif t == 'F' or (nfi and nfi.packType == 'ForeignKeyMap') then
				assert(fkField, 'No foreign key information present')
				assert(t == 'F' or t == 'u' or t == 'i', 'Signature assigns a strange type to a foreign key field: ' .. tostring(t))
				if not fkMap then
					fkMap = {}
					local readInt = t == 'i' and int32_le or uint32_le
					for p=fkField.adOfs, fkField.adOfs+fkField.adLength-1, 8 do
						local v, idx = readInt(data, p), readInt(data, p+4)
						fkMap[idx] = v
					end
				end
				p[#p+1], adv = 'fkMap[ii or (i-1)]', nfi and nfi.packType == 'ForeignKeyMap'
			elseif (t == 'u' or t == 'i') and fsz == 0 and nfi and nfi.bitSize and nfi.bitOffset then
				local se = nfi.packType == 1 and t == 'i'
				p[#p+1], adv = cf == idf and 'lid' or ('%s(data, %d, r0*8+%d)'):format(se and "pint_le" or "upint_le", nfi.bitSize, nfi.bitOffset), 1
				if nfi.packType == 3 or nfi.packType == 4 then
					local extraSkip, aw = nfi.packType == 4 and 4*(cf-nfi.firstFieldIndex) or 0, nfi.packType == 4 and 4*nfi.pa3 or 4
					p[#p] = ('%s(data, 4, %d+%d*%s) %% %d'):format(t == 'u' and 'uint_le' or 'int_le',
						nfi.adOfs+extraSkip, aw, p[#p], 2^nfi.pa2)
				end
			elseif t == 'u' then
				assertLEQ(1, fsz, "Unacceptable field size (u)")
				p[#p+1], adv = cf == idf and 'lid' or ('uint_le(data, ' .. fsz .. ', ' .. cr .. skip .. ')'), 1
			elseif t == 'i' then
				assertLEQ(1, fsz, "Unacceptable field size (i)")
				p[#p+1], adv = 'int_le(data, ' .. fsz .. ', ' .. cr .. skip .. ')', 1
			elseif t == 'f' then
				if nfi.packType == 1 then
					p[#p+1], adv = ('u32_float(upint_le(data, %d, r0*8+%d))'):format(nfi.bitSize, nfi.bitOffset), 1
				elseif nfi.packType == 3 or nfi.packType == 4 then
					local extraSkip, aw = nfi.packType == 4 and 4*(cf-nfi.firstFieldIndex) or 0, nfi.packType == 4 and 4*nfi.pa3 or 4
					p[#p+1], adv = ('upint_le(data, %d, r0*8+%d)'):format(nfi.bitSize, nfi.bitOffset), 1
					p[#p] = ('u32_float(uint_le(data, 4, %d+%d*%s))'):format(nfi.adOfs+extraSkip, aw, p[#p])
				else
					assertLEQ(1, fsz, "Unacceptable field size (f)")
					p[#p+1], adv = 'float32_le(data, ' .. cr .. skip .. ')', 1
				end
			elseif t == 'L' then
				assert(fsz == 8, "Unacceptable field size (L)")
				p[#p+1], adv = '{ uint_le(data, 4, ' .. cr .. skip .. '), uint_le(data, 4, ' .. cr .. skip .. '+4), m=2^32}', 1
			elseif t == 's' then
				if header.inlineStrings then
					p[#p+1], adv = 's' .. crn, 1
					po[#po+1], cr, crn = ('\t\tlocal s%d, r%d = smatch(data, "(%%Z*)()", 1+%s%d)'):format(crn, crn, cr, skip, crn, crn), 'r'..crn..'+', crn + 1
					skip, fsz = 0, 0
				else
					assert(sbase, "invalid signature: 's' requires a string block")
					local base = rowRelativeStrings and cr .. (skip+1) .. (stringShift ~= 0 and " + " .. stringShift or "") or 'sbase'
					local expr = base .. ' + uint_le(data, 4, ' .. cr .. skip .. ')'
					if stringBlockMap then
						expr = 'sbm(' .. expr .. ')'
					end
					p[#p+1], adv = 'smatch(data, "%Z*", ' .. expr .. ')', 1
				end
			else
				error('Unknown signature field type "' .. t .. '"')
			end
			if adv then
				cf, nfi = cf + 1, fi and fi[cf+1]
				skip, fsz = skip + fsz, nfi and nfi[1] or 4
			end
		end
	end
	assert(openTables == 0, 'invalid signature: missing closing table marker' .. (openTables > 1 and "s" or ""))
	if not loose then
		local grace = fkField and 1 or 0
		assertLEQ(tFields, cf-1+grace, 'invalid signature: too few fields specified')
		assertLEQ(skip, header.stride, 'invalid signature: field length exceeds stride')
	end
	
	po[#po+1] = '\t\treturn '
	local code = table.concat(po, '\n') .. table.concat(p, ", ") .. '\n\tend\nend'
	local minId = (header.minId or 0) == 0 and 1 or header.minId
	defaultOverrides = next(defaultOverrides) and defaultOverrides or nil
	return (loadstring or load)(code)(string.match, uint_le, int_le, float32_le, upint_le, pint_le, u32_float,
			header.coVals, header.idMap, copy, rows, stride, sbase, rbase - stride, header.rowList, minId, defaultOverrides, fkMap,
			stringBlockMap
	), fi and (header.stride - (fields+1-cf)) or skip
end

local headerParsers do
	local function dbc(data)
		local h = {rowBase=20}
		h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
		assertLEQ(20 + h.rows*h.stride + h.stringLength, #data, "DBC data too short")
		h.stringBase = h.rowBase + h.rows*h.stride + 1
	
		return h
	end
	local function db2(data)
		local h = {rowBase=48}
		h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
		h.build, h.minId, h.maxId, h.locale = uint32_le(data, 24), uint32_le(data, 32), uint32_le(data, 36), uint32_le(data, 40)
	
		if h.maxId > 0 then
			local n, p, idMap = h.maxId-h.minId + 1, h.rowBase, {}
			h.idMap, h.rowBase = idMap, h.rowBase + 6 * n
			for i=1,n do
				idMap[i], p = uint32_le(data, p), p + 6
			end
		end
		assertLEQ(h.rowBase + h.rows*h.stride + h.stringLength, #data, "DB2 data too short")
		h.stringBase = h.rowBase + h.rows*h.stride + 1
	
		return h
	end
	local db56 do
		local DB6_coType = {[0]=4, [1]=2, [2]=1, [3]='f', [4]=4, [5]=8}
		function db56(data)
			local is6 = data:sub(4,4) == '6'
			local h, rkField, idField = {}
			h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
			h.build, h.minId, h.maxId, h.locale = uint32_le(data, 24), uint32_le(data, 28), uint32_le(data, 32), uint32_le(data, 36)
			h.cloneLength, h.flags, rkField = uint32_le(data, 40), uint16_le(data, 44), 1+uint16_le(data, 46)

			local hend = is6 and 56 or 48
			local hsize, ofsSize, idmSize = hend + 4*h.fields, h.flags % 2 > 0 and 6*(h.maxId-h.minId+1) or 0, h.flags % 8 > 3 and 4*h.rows or 0
			local stSize, stringEnd = ofsSize > 0 and 0 or h.stringLength
			assertLEQ(hsize + h.rows*h.stride + stSize + ofsSize + idmSize, #data, "DB5 data too short")
		
			if ofsSize > 0 and h.rows > 0 then
				local ot, nr, pos, minLen, maxLen, sz = {}, 1, h.stringLength, math.huge, -math.huge
				for i=h.minId, h.maxId do
					pos, sz = pos + 6, uint16_le(data, pos+4)
					if sz > 0 then
						ot[nr], nr = {i, uint32_le(data, pos-6), sz}, nr + 1
						minLen, maxLen = minLen < sz and minLen or sz, maxLen > sz and maxLen or sz
					end
				end
				h.rows, h.rowList, h.maxRowSize, h.minRowSize, h.inlineStrings, stringEnd = #ot, ot, maxLen, minLen, minLen ~= maxLen, h.stringLength + ofsSize
			else
				h.stringBase, stringEnd = hsize + h.rows*h.stride+1, hsize + h.rows*h.stride+stSize
				if idmSize > 0 then
					local idMap, p = {}, h.stringBase+h.stringLength-1
					for i=1,idMap and h.rows or 0 do
						idMap[i], p = uint32_le(data, p), p + 4
					end
					h.idMap = idMap
				end
			end
			h.rowBase, h.cloneOffset = hsize, h.cloneLength > 0 and stringEnd + ofsSize + idmSize or nil
		
			local finfo, p = {}, hend
			for i=1,h.fields do
				local f, sz, o, no = true, (32-int_le(data, 2, p))/8, uint16_le(data, p+2)
				p, no, idField = p + 4, i == h.fields and h.stride or uint16_le(data, p+6), i == rkField and #finfo+1 or idField
				assert(i == 1 or o >= finfo[#finfo][2]+finfo[#finfo][1])
				repeat
					finfo[#finfo+1], o, f = {sz, o, f}, o + sz, false
				until (o+sz) > no
			end
			h.idField = not h.rowList and not h.idMap and idField or nil
			h.inlineStrings = ofsSize > 0 and (h.maxRowSize ~= h.minRowSize or h.maxRowSize > finfo[#finfo][1]+finfo[#finfo][2]) or nil
			if h.rows > 0 and not finfo[#finfo][3] and finfo[#finfo][1] < 4 then
				local md, rl = 0, h.rowList
				for i=#finfo,1,-1 do
					local f = finfo[i]
					if f[3] then break end
					md = md + f[1]
				end
				for i=1,h.rows do
					local re = rl and (rl[i][2]+rl[i][3]) or (hsize+i*h.stride)
					md = #data:sub(re-md+1,re):match("%z*$")
					if md == 0 then break end
				end
				while md >= finfo[#finfo][1] do
					md, h.dropPadding, finfo[#finfo] = md - finfo[#finfo][1], (h.dropPadding or 0) + finfo[#finfo][1]
				end
			end
		
			local coSize = is6 and uint32_le(data, 52) or 0
			if coSize > 0 then
				local _totalFields = uint32_le(data, 48)
				local cobase, p, nc = stringEnd + ofsSize + idmSize + h.cloneLength
				h.coVals, h.coTypes, p, nc = {}, '', cobase + 4, uint32_le(data, cobase)
				for i=1, nc do
					local n, t, c = uint32_le(data, p), data:byte(p+5), {}
					h.coVals[i], c.type, p = c, t, p + 5
					local tt = DB6_coType[t]
					if tt == 'f' then
						c.ttype = 'f'
						for i=1,n do
							c[uint32_le(data, p)], p = float32_le(data, p+4), p + 8
						end
					else
						c.ttype = 'i'
						for i=1,n do
							c[uint32_le(data, p)], p = int_le(data, tt, p+4), p + 4 + tt
						end
					end
					h.coTypes = h.coTypes .. c.ttype
				end
			end
		
			h.fieldInfo, h.fields = finfo, #finfo
			return h
		end
	end
	local dc12 do
		local identityMap = setmetatable({}, {__index=function(_,k) return k end})
		local dc1Map = { [40]=44, [42]=46, [44]=48, [48]=52, [52]=56, [56]=68, [60]=72, [64]=76, [92]=40, [96]=60, [100]=64, [104]=80 }
		function dc12(data)
			local isDC1 = data:sub(4,4) == '1'
			local h, om, feat = {}, isDC1 and dc1Map or identityMap, {}
			h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
			h.minId, h.maxId, h.locale = uint32_le(data, 28), uint32_le(data, 32), uint32_le(data, 36)
			h.flags = uint16_le(data, om[40])
			h.fields2, h.packStart, h.lookupCount = uint32_le(data, om[44]), uint32_le(data, om[48]), uint32_le(data, om[52])
			local cloneLength = uint32_le(data, om[92])
			local keyColumnID = 1+uint16_le(data, om[42])
			local rowListOffset = uint32_le(data, om[96])
			local idmLength = uint32_le(data, om[100])
			local packLength = uint32_le(data, om[56])
			local codLength = uint32_le(data, om[60])
			local palLength = uint32_le(data, om[64])
			local relLength = uint32_le(data, om[104])
			local staticHeaderEnd = (isDC1 and 84 or 108)
			local headerEnd = staticHeaderEnd + 4*h.fields
			h.rowBase = isDC1 and headerEnd or uint32_le(data, 80)
		
			h.stringBase = h.rowBase + h.rows*h.stride + 1
			setFlags(feat,
				packLength > 0 and "PackInfo", idmLength > 0 and "RowIDMap", codLength > 0 and "DefaultVals",
				palLength > 0 and "EnumFields", relLength > 0 and "ForeignKeys", rowListOffset > 0 and "OffsetMap",
				cloneLength > 0 and "CloneRows", h.stringLength > 2 and "StringBlock"
			)
			keyColumnID = idmLength == 0 and keyColumnID or nil

			local mainDataEnd = rowListOffset == 0 and (h.rowBase + h.rows*h.stride + h.stringLength) or (rowListOffset + (h.maxId - h.minId+1)*6)
			local auxDataOffset = mainDataEnd+idmLength+cloneLength+packLength
			local auxDataEnd = auxDataOffset+palLength+codLength
			local fileEnd = auxDataEnd + relLength
			if not isDC1 then
				fileEnd = mainDataEnd + idmLength + cloneLength + relLength
				if h.stringLength > 0 then
					h.rowRelativeStrings = true
				end
			end
		
			assert(h.fields == h.fields2, "DC1 header fields/fields2 values are inconsistent")
			assertLEQ(fileEnd, #data, "DC1 data too short")
			assert(isDC1 or uint32_le(data, 68) == 1, "DC2 multi-part tables are not supported")
		
			assert(packLength == 0 or packLength/24 == h.fields, "DC1 packing data size invalid")
			local packPos = packLength > 0 and (isDC1 and (mainDataEnd + idmLength + cloneLength) or (headerEnd))
			local adOffsetPA = packPos and (isDC1 and (auxDataOffset) or (packPos + packLength))
			local adOffsetCO = packPos and (adOffsetPA+palLength)
			local finfo, extraArrayFields, oddSizedFields, basicArrayFields = {}, 0, 0, 0
			for i=1, h.fields do
				local fi, sz, ofs = {}, 32-int_le(data, 2, staticHeaderEnd-4 + 4*i), uint16_le(data, staticHeaderEnd-2 + 4*i)
				finfo[#finfo+1], fi[1], fi[2] = fi, sz/8, ofs
				if packPos then
					fi.bitOffset, fi.bitSize, fi.adLength = uint16_le(data, packPos), uint16_le(data, packPos+2), uint32_le(data, packPos+4)
					fi.packType, fi.pa1, fi.pa2, fi.pa3 = uint32_le(data, packPos+8), uint32_le(data, packPos+12), uint32_le(data, packPos+16), uint32_le(data, packPos+20)
					packPos = packPos + 24
					if not (fi.packType == 0 or sz == 0 or fi.bitSize == sz) then
						error(('DC1 field/packing width mismatch: field %d, pack %d, sz %d, bsz %d'):format(i, fi.packType, sz, fi.bitSize))
					end
					if fi.packType == 0 and fi.bitSize ~= sz and sz > 0 then
						assert(fi.bitSize % sz == 0, 'DC1 array field width parity check')
						basicArrayFields = basicArrayFields + (fi.bitSize/sz-1)
						for j=2,fi.bitSize/sz do
							finfo[#finfo+1] = {sz/8, ofs+sz*(j-1)}
						end
					elseif fi.adLength == 0 then
					elseif fi.packType == 2 then
						fi.adOfs, adOffsetCO = adOffsetCO, adOffsetCO + fi.adLength
					else
						assert(fi.packType == 3 or fi.packType == 4, "Unknown DC1 field packing type: " .. fi.packType)
						fi.adOfs, adOffsetPA = adOffsetPA, adOffsetPA + fi.adLength
						if fi.packType == 4 and fi.pa3 >= 1 then
							fi.firstFieldIndex, extraArrayFields = #finfo, extraArrayFields + fi.pa3 - 1
							for i=2,fi.pa3 do
								finfo[#finfo+1] = fi
							end
						end
					end
					oddSizedFields = oddSizedFields + (fi.packType ~= 0 and 1 or 0)
				end
				if i == keyColumnID then
					h.idField = #finfo
				end
			end
			feat.ArrayFields = basicArrayFields > 0
			feat.DictArrayFields = extraArrayFields > 0
			feat.PackedFields = oddSizedFields > 0
		
			if cloneLength > 0 then
				assert(cloneLength % 8 == 0, 'DC1 clone instructions length parity check')
				h.cloneOffset, h.cloneLength = mainDataEnd + idmLength, cloneLength
			end
			if relLength > 0 then
				local p = isDC1 and auxDataEnd or (mainDataEnd + idmLength + cloneLength)
				local count, min, max = uint32_le(data, p), uint32_le(data, p+4), uint32_le(data, p+8)
				h.fkField = {0,0, packType="ForeignKeyMap", adOfs=p+12, adLength=relLength-12, pa1=count, pa2=min, pa3=max}
				finfo[#finfo+1] = h.fkField
			end
		
			-- We don't treat arrays as single fields around here, so lie about the field count in the header
			-- (h.fields2 preserves the original count)
			h.fieldInfo, h.fields = finfo, #finfo
		
			if rowListOffset > 0 and h.rows > 0 then
				local ot, nr, pos, minLen, maxLen, sz = {}, 1, rowListOffset, math.huge, -math.huge
				for i=h.minId, h.maxId do
					pos, sz = pos + 6, uint16_le(data, pos+4)
					if sz > 0 then
						ot[nr], nr = {i, uint32_le(data, pos-6), sz}, nr + 1
						minLen, maxLen = minLen < sz and minLen or sz, maxLen > sz and maxLen or sz
					end
				end
				h.rows, h.rowList, h.maxRowSize, h.minRowSize, h.inlineStrings = #ot, ot, maxLen, minLen, minLen ~= maxLen
				feat.InlineStrings = h.inlineStrings
			elseif idmLength > 0 then
				local idMap, p = {}, mainDataEnd
				for i=1, h.rows do
					idMap[i], p = uint32_le(data, p), p + 4
				end
				h.idMap = idMap
			end
			h.featureDesc = serializeFlags(feat)

			if h.inlineStrings then
				h.rowRelativeStrings = nil
			end
		
			return h
		end
	end
	local function dc34(data)
		local is4 = data:sub(1,4) == 'WDC4'
		local h, feat = {parts={}}, {}
		h.rows, h.fields, h.stride, h.stringLength = uint32_le(data, 4), uint32_le(data, 8), uint32_le(data, 12), uint32_le(data, 16)
		h.minId, h.maxId, h.locale = uint32_le(data, 28), uint32_le(data, 32), uint32_le(data, 36)
		h.flags = uint16_le(data, 40)
		h.fields2, h.packStart, h.lookupCount = uint32_le(data, 44), uint32_le(data, 48), uint32_le(data, 52)
		local keyColumnID = 1+uint16_le(data, 42)
		local packLength = uint32_le(data, 56)
		local codLength = uint32_le(data, 60)
		local palLength = uint32_le(data, 64)
		local numParts = uint32_le(data, 68)
		local fieldInfoPos = 72 + numParts*40
		local packPos = packLength > 0 and (fieldInfoPos + 4*h.fields)
		assert(h.fields == h.fields2, "DC3/4 header fields/fields2 values are inconsistent")
		assert(packLength == 0 or packLength/24 == h.fields, "DC3/4 packing parity check")
		local eidLength, eidPos = 0, (packPos or fieldInfoPos) + packLength + palLength + codLength
		
		local hasExternalPrimaryID, hasStringBlock, hasInlineStrings, hasForeignKey = false, false, false, false
		local partMeta = {__index=h}
		for i=1, numParts do
			local p = 32+i*40
			local pt = {
				partIndex=i,
				keyName = ("%02x%02x%02x%02x%02x%02x%02x%02x"):format(data:byte(p+1, p+8)),
				rowBase = uint32_le(data, p+8),
				rows = uint32_le(data, p+12),
				stringLength = uint32_le(data, p+16),
				rowsEnd = uint32_le(data, p+20),
				idmLength = uint32_le(data, p+24),
				relLength = uint32_le(data, p+28),
				rowListEntries = uint32_le(data, p+32),
				cloneLength = uint32_le(data, p+36)*8,
			}
			if pt.rowsEnd > 0 then
				p = pt.rowsEnd
				pt.rowsLength = p - pt.rowBase
			else
				pt.rowsLength = h.stride * pt.rows
				p = pt.rowBase + pt.rowsLength + pt.stringLength
				pt.stringBase = p - pt.stringLength + 1
				pt.rowRelativeStrings = true
				hasStringBlock = hasStringBlock or (pt.stringLength > 0)
			end
			if pt.idmLength > 0 then
				pt.idmOffset, hasExternalPrimaryID, p = p, true, p + pt.idmLength
			end
			if pt.cloneLength > 0 then
				assert(pt.cloneLength % 8 == 0, 'DC3/4 row cloning length parity check')
				pt.cloneOffset, p = p, p + pt.cloneLength
			end
			if pt.rowListEntries > 0 then
				pt.rowListOffset, hasExternalPrimaryID, p = p, true, p + pt.rowListEntries*6
			end
			if pt.relLength > 0 then
				pt.relBase, p = p, p + pt.relLength
			end
			if pt.rowListEntries > 0 then
				pt.rowIDListOffset, p = p, p + pt.rowListEntries*4
			end
			pt.partEndOffset = p
			pt.isPresent = (data:match("()%Z", pt.rowBase) or pt.partEndOffset) < pt.partEndOffset
			assertLEQ(pt.partEndOffset, #data, "DC3/4 data too short")
			
			if pt.isPresent then
				if pt.rowListOffset and pt.rows > 0 then
					local lp, ip = pt.rowListOffset, pt.rowIDListOffset
					local ot, minLen, maxLen, sz = {}, math.huge, -math.huge
					for i=1, pt.rowListEntries do
						lp, sz = lp + 6, uint16_le(data, lp+4)
						if sz <= 0 then error('DC3/4 unexpected zero-length row') end
						ot[i], ip = {uint32_le(data, ip), uint32_le(data, lp-6), sz}, ip + 4
						minLen, maxLen = minLen < sz and minLen or sz, maxLen > sz and maxLen or sz
					end
					-- When all inline strings in the part are of the same length, this doesn't work.
					-- There's normally enough variation /somewhere/, so this sets the flag globally
					-- via the h.inlineStrings __index if any part has length variability.
					pt.rows, pt.rowList, pt.maxRowSize, pt.minRowSize, pt.inlineStrings = #ot, ot, maxLen, minLen, minLen ~= maxLen or nil
					hasInlineStrings = hasInlineStrings or pt.inlineStrings
					
					if M.PREFER_ERRORS and pt.idmLength > 0 then
						local idmd = data:sub(pt.idmOffset+1, pt.idmOffset+pt.idmLength)
						local rlid = data:sub(pt.rowIDListOffset+1, pt.rowIDListOffset+4*pt.rowListEntries)
						assert(idmd == rlid, "DC3/4 primary key disagreement")
					else
						feat.IgnoredIDMaps = feat.IgnoredIDMaps or pt.idmLength > 0
					end
				elseif pt.idmLength > 0 then
					local idMap, p = {}, pt.idmOffset
					for i=1, pt.rows do
						idMap[i], p = uint32_le(data, p), p + 4
					end
					pt.idMap = idMap
				end
				if pt.relLength > 0 then
					local p = pt.relBase
					local count, min, max = uint32_le(data, p), uint32_le(data, p+4), uint32_le(data, p+8)
					pt.fkField, hasForeignKey = {0,0, packType="ForeignKeyMap", adOfs=p+12, adLength=pt.relLength-12, pa1=count, pa2=min, pa3=max}, true
				end
			else
				feat.MissingParts = true
			end
			
			if is4 and pt.keyName ~= "0000000000000000" then
				local eidCount = uint32_le(data, eidPos)
				local eidPartLength = 4 + eidCount * 4
				if eidCount < pt.rows and M.PREFER_ERRORS then
					error("DC4 encrypted ID list underflow (part header declares more rows)")
				end
				eidLength, eidPos = eidLength + eidPartLength, eidPos + eidPartLength
			end
			
			h.parts[i] = setmetatable(pt, partMeta)
		end
		for i=1, numParts do
			assertLEQ(eidPos, h.parts[i].rowBase, "DC3/4 part data in header region")
		end
		if hasStringBlock then -- addressing
			-- Use stringShift to map field-relative references to indices into a unified string block...
			local sbi, s, sn = {}, 0, 1
			for i=#h.parts, 1, -1 do
				local p = h.parts[i]
				p.stringShift, s = -s - p.stringBase, s + p.rowsLength
			end
			for i=1,#h.parts do
				local p = h.parts[i]
				if p.stringLength > 0 then
					sbi[sn], sbi[sn+1], sn = p.stringLength + (sbi[i+i-3] or 0), p.stringBase - (sbi[i+i-3] or 0), sn+2
				end
			end
			-- ... then convert unified string block indices to indices into the original data.
			if #sbi > 0 then
				sbi[#sbi+1], sbi[#sbi+2] = math.huge, #data+9e12+1
				local lastLow, lastHigh, lastShift, invalid = 0, sbi[1], sbi[2], sbi[#sbi]
				h.mapStringOffset = function(o)
					if o < 0 then
						return invalid
					elseif o < lastLow or o >= lastHigh then
						for i=1,#sbi,2 do
							local si = sbi[i]
							if si > o then
								lastLow, lastHigh, lastShift = sbi[i-2] or 0, si, sbi[i+1]
								break
							end
						end
					end
					return o + lastShift
				end
			end
		end
		if M.PREFER_ERRORS and hasInlineStrings and hasStringBlock then
			error("DC3/4 decoder does not support mixing in-line strings and string blocks")
		end
		h.inlineStrings = hasInlineStrings
		
		setFlags(feat,
			packLength > 0 and "PackInfo", codLength > 0 and "DefaultVals",
			palLength > 0 and "EnumFields", h.stringLength > 2 and "StringBlock",
			hasInlineStrings and "InlineStrings", hasInlineStrings and hasStringBlock and "MixedStrings",
			hasForeignKey and "ForeignKeys", h.lookupCount > 0 and "Lookup[" .. h.lookupCount .. "]",
			#h.parts > 1 and ("Parts[" .. #h.parts .. "]")
		)
		keyColumnID = (not hasExternalPrimaryID) and keyColumnID or nil

		local adOffsetPA = packPos and (packPos + packLength)
		local adOffsetCO = packPos and (adOffsetPA+palLength)
		local finfo, extraArrayFields, oddSizedFields, basicArrayFields = {}, 0, 0, 0
		for i=1, h.fields do
			local fi, sz, ofs = {}, 32-int_le(data, 2, fieldInfoPos-4 + 4*i), uint16_le(data, fieldInfoPos-2 + 4*i)
			finfo[#finfo+1], fi[1], fi[2] = fi, sz/8, ofs
			if packPos then
				fi.bitOffset, fi.bitSize, fi.adLength = uint16_le(data, packPos), uint16_le(data, packPos+2), uint32_le(data, packPos+4)
				fi.packType, fi.pa1, fi.pa2, fi.pa3 = uint32_le(data, packPos+8), uint32_le(data, packPos+12), uint32_le(data, packPos+16), uint32_le(data, packPos+20)
				packPos = packPos + 24
				if not (fi.packType == 0 or sz == 0 or fi.bitSize == sz) then
					error(('DC3/4 field/packing width mismatch: field %d, pack %d, sz %d, bsz %d'):format(i, fi.packType, sz, fi.bitSize))
				end
				if fi.packType == 0 and fi.bitSize ~= sz and sz > 0 then
					assert(fi.bitSize % sz == 0, 'DC3/4 array field width parity check')
					basicArrayFields = basicArrayFields + (fi.bitSize/sz-1)
					for j=2,fi.bitSize/sz do
						finfo[#finfo+1] = {sz/8, ofs+sz*(j-1)}
					end
				elseif fi.adLength == 0 then
				elseif fi.packType == 2 then
					fi.adOfs, adOffsetCO = adOffsetCO, adOffsetCO + fi.adLength
				else
					assert(fi.packType == 3 or fi.packType == 4, "DC3/4 field packing type " .. tostring(fi.packType) .. " not implemented")
					fi.adOfs, adOffsetPA = adOffsetPA, adOffsetPA + fi.adLength
					if fi.packType == 4 and fi.pa3 >= 1 then
						fi.firstFieldIndex, extraArrayFields = #finfo, extraArrayFields + fi.pa3 - 1
						for i=2, fi.pa3 do
							finfo[#finfo+1] = fi
						end
					end
				end
				oddSizedFields = oddSizedFields + (fi.packType ~= 0 and 1 or 0)
			end
			if i == keyColumnID then
				h.idField = #finfo
			end
		end
		if hasForeignKey then
			finfo[#finfo+1] = {0,0, packType="ForeignKeyMap"}
		end
		feat.ArrayFields = basicArrayFields > 0
		feat.DictArrayFields = extraArrayFields > 0
		feat.PackedFields = oddSizedFields > 0
		if hasInlineStrings then
			h.rowRelativeStrings = nil
		end
		
		-- update h.fields to match the expanded field count; h.fields2 retains the header value
		h.fieldInfo, h.fields = finfo, #finfo
		h.featureDesc = serializeFlags(feat)
		
		return h
	end
	headerParsers = {
		WDBC=dbc, WDB2=db2, WCH2=db2,
		WDB5=db56, WDB6=db56,
		WDC1=dc12, WDC2=dc12, ["1SLC"]=dc12,
		WDC3=dc34, WDC4=dc34,
	}
end

function M.header(data)
	local fourCC = data:sub(1,4)
	local hp = headerParsers[fourCC] or error(("Unsupported DBC format %q"):format(fourCC))
	return hp(data)
end

function M.rows(data, sig, loose)
	assert(type(data) == "string" and type(sig) == "string", 'Syntax: casc.dbc.rows("data", "rowSignature"[, loose])')
	
	local h = M.header(data)
	local iter = createUnpacker(data, h, sig, loose)

	return iter, data
end

function M.fields(data, sig)
	assert(type(data) == "string", 'Syntax: casc.dbc.fields("data"[, "rowSignature"])')
	return guessTypes(data, M.header(data), sig)
end

return M