local project, branch, fileType = ...

--luacheck: globals require assert
--luacheck: globals tostring setmetatable
--luacheck: globals table io next

local function write(text, ...)
    _G.print(text:format(...))
end

local progress = 0
local function UpdateProgress(current)
    if (current - progress) > 0.1 then
        write("%d%%", current * 100)
        progress = current
    end
    if current == 1 then
        write("Done!")
        progress = 0
    end
end

local projects = {
    retail = "wow",
    classic = "wow_classic",
}
local branches = {
    live = "",
    ptr = "t",
    beta = "_beta",
    alpha = "dev",

    -- Classic PTR
    ptrC = "_ptr",
}

if project then
    if branches[project] then
        project, branch = "retail", project
    end

    if not branch then
        branch = "live"
    end

    if project == "classic" and branch == "ptr" then
        branch = "ptrC"
    end
else
    project, branch = "retail", "live"
end

write("Extracting project %s on branch %s...", project, branch)
local product = projects[project] .. branches[branch]

fileType = fileType or "Code"


local casc = require("casc")
local plat = require("casc.platform")
local dbc = require("dbc")
local csv = require("csv")
local lfs = require("lfs")

local WOWDIR = "E:/World of Warcraft"
local CDN = ("http://us.patch.battle.net:1119/%s/#us"):format(product)
local FILEID_PATH_MAP = {
    ["DBFilesClient/ManifestInterfaceData.db2"] = 1375801,
    ["DBFilesClient/GlobalStrings.db2"] = 1394440,
    ["DBFilesClient/UiTextureAtlas.db2"] = 897470,
    ["DBFilesClient/UiTextureAtlasMember.db2"] = 897532,
}

local conf do
    local function selectBuild(buildInfo)
        for i = 1, #buildInfo do
            --print(buildInfo[i].Product, buildInfo[i].Active)
            if buildInfo[i].Product == product and buildInfo[i].Active == 1 then
                return i
            end
            assert(0)
        end
    end

    local buildKey, _, _, version = casc.localbuild(WOWDIR .. "\\.build.info", selectBuild)
    _G.print(("Build: %s"):format(tostring(version)))

    conf = {
        base = WOWDIR .. "/data",
        locale = casc.locale.US,
        verifyHashes = false,
        bkey = buildKey,
        --log = print
    }
end
local fileHandle = assert(casc.open(CDN, conf))

local files = {}
local fileFilter = {
    xml = "Code",
    lua = "Code",
    toc = "Code",
    xsd = "Code",

    blp = "Art"
}

--[[ Filter Names ]]--
local params = {
    header = true,
}
local csvFile = csv.open("manifestinterfacedata.csv", params)
if csvFile then
    -- https://wow.tools/dbc/api/export/?name=manifestinterfacedata&build=8.3.0.33369
    for fields in csvFile:lines() do
        local name = fields.FileName
        local path = (fields.FilePath):gsub("[/\\]+", "/")
        if fileFilter[(name:match("%.(...)$") or ""):lower()] == fileType then
            files[#files + 1] = {
                path = path,
                name = name,
                fullPath = path .. name,
            }
        end
    end
else
    fileHandle.root:addFileIDPaths(FILEID_PATH_MAP)

    local fileData = assert(fileHandle:readFile("DBFilesClient/ManifestInterfaceData.db2"))
    for _, path, name in dbc.rows(fileData, "ss") do
        if path:match("^[Ii][Nn][Tt][Ee][Rr][Ff][Aa][Cc][Ee][\\/]") then
            --print(_, path, name)
            if fileFilter[(name:match("%.(...)$") or ""):lower()] == fileType then
                path = path:gsub("[/\\]+", "/")
                files[#files + 1] = {
                    path = path,
                    name = name,
                    fullPath = path .. name,
                }
            end
        end
    end
end


local l = setmetatable({}, {__index=function(s, a) s[a] = a:lower() return s[a] end})
table.sort(files, function(a, b)
    return l[a.fullPath] < l[b.fullPath]
end)

--[[ Create Files ]]--
local dirs = {}
for i = 1, #files do
    local path = files[i].fullPath
    --print("file", path)
    for endPoint in path:gmatch("()/") do
        local subPath = path:sub(1, endPoint - 1)
        local subLower = subPath:lower()
        --print("path", path, subPath)

        if not dirs[subLower] then
            --print("dir", subLower, subPath)
            dirs[subLower] = subPath
        end
    end
end

local root, makeDirs = "BlizzardInterface" .. fileType, {}
for _, subPath in next, dirs do
    table.insert(makeDirs, subPath)
end
table.sort(makeDirs, function(a, b)
    return #a < #b
end)

write("Creating %d folders...", #makeDirs)
plat.mkdir(root)
for i = 1, #makeDirs do
    --print("make dir", root, makeDirs[i])
    UpdateProgress(i / #makeDirs)
    plat.mkdir(plat.path(root, makeDirs[i]))
end

write("Creating %d files...", #files)
local fails = {}
for i = 1, #files do
    UpdateProgress(i / #files)
    local file = files[i]
    local filePath = file.fullPath
    local fixedCase = (filePath:gsub("[^/]+()/", function(b)
        local s = filePath:sub(1, b - 1)
        return dirs[s:lower()]:match("([^/]+/)$")
    end))
    local w = fileHandle:readFile(filePath)
    if w then
        --print("create", root, fixedCase)
        local h = io.open(plat.path(root, fixedCase), "wb")
        h:write(w)
        h:close()
    else
        fails[file.path:lower()] = file.path
    end
end


if next(fails) then
    local file = assert(io.open("fails.txt", "w"))
    for _, path in next, fails do
        --print("failed", path)
        file:write(path, "\n")
        assert(lfs.rmdir(plat.path(root, path)))
    end
end
