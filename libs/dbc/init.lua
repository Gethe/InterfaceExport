-- SPDX-FileCopyrightText: © 2024 foxlit <https://www.townlong-yak.com/dbc/>
-- SPDX-License-Identifier: Artistic-2.0

local M, bin = {MAX_GUESS_ATTEMPTS=50, TARGET_TEST_SET_SIZE=100, PREFER_ERRORS=true, SCAN_PACK_WIDTH=true, _VERSION="LuaDBC 1.15"}, require("dbc.bin")

local uint32_le, int32_le, float32_le = bin.uint32_le, bin.int32_le, bin.float32_le
local int_le, uint_le = bin.int_le, bin.uint_le
local pint_le, upint_le, u32_float = bin.pint_le, bin.upint_le, bin.u32_float

local POSSIBLE_FLOAT_PACK_TYPES = {[1]=true, [3]=true, [4]=true}
local FIELD_TYPE_BASE_EQUIV = {I="i", U="u"}
local FIELD_TYPE_ALLOW_NARROW = {i=1, u=1}

local function assertLEQ(a, b, message)
	if a > b then
		error(message .. ": " .. a .. " > " .. b, 2)
	end
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
local function updateFloatScores(ic, fc, _iv, fv)
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
local function findLargestPart(header)
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
	return largePartID, numPresentParts
end
local function createUnpacker(data, header, format, loose, partID, guessedTypes)
	if header.parts ~= nil and partID == nil then
		local largePartID, numPresentParts = findLargestPart(header)
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
			local t, pt, adv, sigType = t == '?' and gt:sub(cf, cf) or t, nfi and nfi.packType
			t, sigType = FIELD_TYPE_BASE_EQUIV[t] or t, t
			if cf == idf and fsz <= 5 and missingLocalRowIndex then
				if fsz == 0 and nfi and nfi.bitSize and nfi.bitOffset then
					local se = pt == 1 and (nfi.pa3 and nfi.pa3 % 2 == 1)
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
			elseif nfi and pt == 2 then
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
			elseif t == 'F' or pt == 'ForeignKeyMap' then
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
				p[#p+1], adv = 'fkMap[ii or (i-1)]', pt == 'ForeignKeyMap'
			elseif (t == 'u' or t == 'i') and fsz == 0 and nfi and nfi.bitSize and nfi.bitOffset then
				local signed = (pt == 1 or pt == 5) and t == 'i'
				p[#p+1], adv = cf == idf and 'lid' or ('%s(data, %d, r0*8+%d)'):format(signed and "pint_le" or "upint_le", nfi.bitSize, nfi.bitOffset), 1
				if pt == 3 or pt == 4 then
					local extraSkip, aw = pt == 4 and 4*(cf-nfi.firstFieldIndex) or 0, pt == 4 and 4*nfi.pa3 or 4
					p[#p] = ('%s(data, %d, %d+%d*%s)'):format(t == 'u' and 'uint_le' or 'int_le', FIELD_TYPE_ALLOW_NARROW[sigType] and nfi.packWidth or 4,
						nfi.adOfs+extraSkip, aw, p[#p])
				end
			elseif t == 'u' then
				assertLEQ(1, fsz, "Unacceptable field size (u)")
				p[#p+1], adv = cf == idf and 'lid' or ('uint_le(data, ' .. fsz .. ', ' .. cr .. skip .. ')'), 1
			elseif t == 'i' then
				assertLEQ(1, fsz, "Unacceptable field size (i)")
				p[#p+1], adv = 'int_le(data, ' .. fsz .. ', ' .. cr .. skip .. ')', 1
			elseif t == 'f' then
				if pt == 1 then
					p[#p+1], adv = ('u32_float(upint_le(data, %d, r0*8+%d))'):format(nfi.bitSize, nfi.bitOffset), 1
				elseif pt == 3 or pt == 4 then
					local extraSkip, aw = pt == 4 and 4*(cf-nfi.firstFieldIndex) or 0, pt == 4 and 4*nfi.pa3 or 4
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

local parsers = {} do
	local modules = {"dbc", "db2", "db5_6", "dc1_2", "dc3_4_5"}
	for i=1,#modules do
		local ok, m = pcall(require, "dbc.headers." .. modules[i])
		if ok and type(m) == "table" and type(m.parseHeader) == "function" and type(m.fourCC) == "table" then
			for fourCC in pairs(m.fourCC) do
				parsers[fourCC] = m
			end
		end
	end
end

function M.header(data)
	assert(type(data) == "string", 'Syntax: header = dbc.header("data")')
	assertLEQ(4, #data, "dbc.header: data too short")
	local hm = parsers[data:sub(1,4)] or error("Unsupported DBC format [" .. (data:match("^%w%w%w%w") or ("%02x%02x%02x%02x"):format(data:byte(1,4))) .. "]")
	return hm:parseHeader(data, M)
end

function M.rows(data, sig, loose, header)
	assert(type(data) == "string" and type(sig) == "string", 'Syntax: dbc.rows("data", "rowSignature"[, loose[, header]])')
	assertLEQ(4, #data, "dbc.rows: data too short")
	
	local h = type(header) == "table" and header or M.header(data)
	local iter = createUnpacker(data, h, sig, loose)

	return iter, data
end

function M.fields(data, sig)
	assert(type(data) == "string" and (sig == nil or type(sig) == "string"), 'Syntax: casc.dbc.fields("data"[, "rowSignature"])')
	local h = M.header(data)
	return guessTypes(data, h.parts == nil and h or h.parts[findLargestPart(h)], sig)
end

return M