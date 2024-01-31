-- SPDX-FileCopyrightText: © 2023 foxlit <https://www.townlong-yak.com/casc/>
-- SPDX-License-Identifier: Artistic-2.0

local dir_sep = package and package.config and package.config:sub(1,1) or "/"
if dir_sep == '/' then
	return require("casc.platform-fallback.unix")
elseif dir_sep == '\\' then
	return require("casc.platform-fallback.windows")
end