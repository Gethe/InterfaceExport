local project, branch, filter = ...

--luacheck: globals require assert pcall type
--luacheck: globals tostring setmetatable
--luacheck: globals table io next os package

local function write(text, ...)
    _G.print(text:format(...))
end

local projects = {
    retail = "wow",
    classic = "wow_classic",
    bcc = "wow_classic",
    classic_era = "wow_classic_era",
    vanilla = "wow_classic_era",
}
local branches = {
    live = "",
    ptr = "t",
    beta = "_beta",

    -- Classic PTR
    ptrC = "_ptr",
}
local fileTypes = {
    all = true,
    code = "Code",
    art = "Art",
}

if project then
    if branches[project] then
        branch, filter = project, branch
        project = "retail"
    end

    if fileTypes[project] then
        filter = project
        project = "retail"
        branch = "live"
    end

    if fileTypes[branch] then
        filter = branch
        branch = "live"
    end

    if project ~= "retail" and branch == "ptr" then
        branch = "ptrC"
    end
else
    project, branch, filter = "retail", "live", "all"
end

filter = filter or "all"

write("Extracting %s from %s %s...", filter, project, branch)
local product = projects[project] .. branches[branch]


local casc = require("casc")
local plat = require("casc.platform")
local dbc = require("dbc")
local csv = pcall(require, "csv")

local WOWDIR = "E:/World of Warcraft"
local CACHE_DIR = "./InterfaceExport/Cache"
local REGION = "us"
local PATCH_BASE = ("http://%s.patch.battle.net:1119/%s"):format(REGION, product)
local FILEID_PATH_MAP = {
    ["DBFilesClient/ManifestInterfaceData.db2"] = 1375801,
    ["DBFilesClient/GlobalStrings.db2"] = 1394440,
    ["DBFilesClient/UiTextureAtlas.db2"] = 897470,
    ["DBFilesClient/UiTextureAtlasMember.db2"] = 897532,
}

local rmdir do
    -- Based on code from casc.platform
    local dir_sep = package and package.config and package.config:sub(1,1) or "/"
    local command = "rmdir %s"
    if dir_sep == '/' then
        -- *nix
        command = "rm -r %s"
    end
    local function execute(...)
        local ok, status, sig = os.execute(...)
        if ok == true and status == "exit" or status == "signal" then
            return sig
        else
            return ok or sig or ok, status, sig
        end
    end
    local function shellEscape(s)
        return '"' .. s:gsub('"', '"\\\\""') .. '"'
    end
    function rmdir(path)
        return execute(command:format(shellEscape(path)))
    end
end

local fileHandle do
    local function selectBuild(buildInfo)
        for i = 1, #buildInfo do
            --print(buildInfo[i].Product, buildInfo[i].Active)
            if buildInfo[i].Product == product and buildInfo[i].Active == 1 then
                return i
            end
            assert(0)
        end
    end

    local base = WOWDIR .. "/Data"
    local buildKey, cdn, ckey, version = casc.localbuild(WOWDIR .. "/.build.info", selectBuild)
    if version then
        write("Product: %s Build: %s", product, tostring(version))
    else
        write("Local build not found, checking CDN...")

        buildKey, cdn, ckey, version = casc.cdnbuild(PATCH_BASE, REGION)
        if version then
            write("CDN Product: %s Build: %s", product, tostring(version))
            base = CACHE_DIR
        else
            write("Product %s not found", product)
            return
        end
    end

    local versionBuild = ("%s (%s)"):format(version:match("(%d+.%d+.%d).(%d*)"))
    if versionBuild then
        local file = assert(io.open("version.txt", "w"))
        file:write(versionBuild, "\n")
    end

    plat.mkdir(CACHE_DIR)
    local conf = {
        bkey = buildKey,
        base = base,
        cdn = cdn,
        ckey = ckey,
        cache = CACHE_DIR,
        cacheFiles = true,
        locale = casc.locale.US,
        requireRootFile = false,
        --verifyHashes = false,
        --log = print
    }

    fileHandle = assert(casc.open(conf))
end

local GetFileList do
    local fileFilter = {
        xml = "code",
        lua = "code",
        toc = "code",
        xsd = "code",

        blp = "art"
    }

    local params = {
        header = true,
    }

    local l = setmetatable({}, {__index=function(s, a) s[a] = a:lower() return s[a] end})
    local function SortPath(a, b)
        return l[a.fullPath] < l[b.fullPath]
    end

    local function CheckFile(fileType, files, id, path, name)
        --print(_, path, name)
        if fileFilter[(name:match("%.(...)$") or ""):lower()] == fileType then
            path = path:gsub("[/\\]+", "/")

            --print("CheckFile", path)
            files[#files + 1] = {
                path = path,
                id = id,
                fullPath = path .. name,
            }
        end
    end

    function GetFileList(fileType)
        local files = {}
        if csv and io.open("manifestinterfacedata.csv", "r")then
            -- from wow.tools table browser
            local csvFile = csv.open("manifestinterfacedata.csv", params)
            for fields in csvFile:lines() do
                CheckFile(fileType, files, fields.ID, fields.FilePath, fields.FileName)
            end
        else
            fileHandle.root:addFileIDPaths(FILEID_PATH_MAP)

            local fileData = assert(fileHandle:readFile("DBFilesClient/ManifestInterfaceData.db2"))
            for id, path, name in dbc.rows(fileData, "ss") do
                if path:match("^[Ii][Nn][Tt][Ee][Rr][Ff][Aa][Cc][Ee][\\/_]") then
                    CheckFile(fileType, files, id, path, name)
                end
            end
        end

        table.sort(files, SortPath)
        return files
    end
end


local progress = 0
local collectgarbage = _G.collectgarbage
local gcLimit = 1024 * 1024
local function UpdateProgress(current)
    if collectgarbage("count") > gcLimit then
        collectgarbage()
    end

    if (current - progress) > 0.1 then
        write("%d%%", current * 100)
        progress = current
    end
    if current == 1 then
        write("Done!")
        progress = 0
    end
end


local CreateDirectories do
    local function SortDirectories(a, b)
        return #a < #b
    end

    function CreateDirectories(files, root)
        local dirs = {}
        for i = 1, #files do
            local path = files[i].fullPath
            for endPoint in path:gmatch("()/") do
                local subPath = path:sub(1, endPoint - 1)
                local subLower = subPath:lower()

                if not dirs[subLower] then
                    --print("dir", path, subPath, subLower)
                    dirs[subLower] = subPath
                end
            end
        end

        local makeDirs = {}
        for _, subPath in next, dirs do
            table.insert(makeDirs, subPath)
        end
        table.sort(makeDirs, SortDirectories)

        write("Creating %d folders...", #makeDirs)
        plat.mkdir(root)
        for i = 1, #makeDirs do
            --print("make dir", root, makeDirs[i])
            plat.mkdir(plat.path(root, makeDirs[i]))
            --UpdateProgress(i / #makeDirs)
        end

        return dirs
    end
end


local ExtractFiles do
    function ExtractFiles(fileType)
        local files = GetFileList(fileType)
        local root = "./"
        if filter == "all" then
            root = "BlizzardInterface" .. fileTypes[fileType]
        end
        local dirs = CreateDirectories(files, root)

        local file, filePath, fixedCase
        local fails, w, h = {}
        local function FixCase(b)
            local s = filePath:sub(1, b - 1)
            --print("fixedCase", filePath, b, s)
            return dirs[s:lower()]:match("([^/]+/)$")
        end

        write("Creating %d files...", #files)
        for i = 1, #files do
            file = files[i]
            filePath = file.fullPath
            fixedCase = (filePath:gsub("[^/]+()/", FixCase))
            w = fileHandle:readFile(file.fullPath)
            if w then
                write("create %s", fixedCase)
                h = io.open(plat.path(root, fixedCase), "wb")
                h:write(w)
                h:close()
            else
                --print("fail", file.path, filePath)
                fails[file.path:lower()] = file.path
            end
            --UpdateProgress(i / #files)
        end


        if next(fails) then
            file = assert(io.open("fails"..fileType..".txt", "w"))
            for pathLower, path in next, fails do
                if rmdir(plat.path(root, path)) then
                    --print("failed", path)
                    file:write(path, "\n")
                end
            end
        end
    end
end


if filter == "all" then
    ExtractFiles("code")
    ExtractFiles("art")
else
    ExtractFiles(filter)
end
