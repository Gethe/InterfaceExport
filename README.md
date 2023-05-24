WoW Interface Export
====================

This tool that will extract the interface files for World of Warcraft, either from a local install or from the CDN.

Usage:
`lua export.lua [project] [branch] [filter]`

### project ###
  * `retail`      - Default
  * `classic`     - Most recent Classic version 

These can also be used to refer to a specific WoW Classic project

  * `wrath`       - WotLK Classic
  * `classic_era` - Vanilla Classic
  * `vanilla`     - Vanilla Classic

### branch ###
  * `live` - Default
  * `ptr`
  * `ptr2` - Retail client only
  * `beta`

### filter ###
  * `all`  - Default
  * `code`
  * `art`
  * `png` - Like `art`, but also converts to png


Tools used:
  * [LuaCasc](https://www.townlong-yak.com/casc/)
  * [LuaDBC](https://www.townlong-yak.com/casc/dbc/)
  * [LuaFileSystem](https://luarocks.org/modules/hisham/luafilesystem)
  * [LuaCSV](https://luarocks.org/modules/geoffleyland/csv) (optional)
