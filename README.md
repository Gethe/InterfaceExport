WoW Interface Export
====================

This tool that will extract the interface files for World of Warcraft, either from a local install or from the CDN.

Usage:
`lua export.lua [project] [branch] [filter]`

### project ###
  * `retail` - Default
  * `classic`

### branch ###
  * `live` - Default
  * `ptr`
  * `beta`

### filter ###
  * `all` - Default
  * `code`
  * `art`


Tools used:
  * [LuaCasc](https://www.townlong-yak.com/casc/)
  * [LuaDBC](https://www.townlong-yak.com/casc/dbc/)
  * [LuaFileSystem](https://luarocks.org/modules/hisham/luafilesystem)
  * [LuaCSV](https://luarocks.org/modules/geoffleyland/csv)
