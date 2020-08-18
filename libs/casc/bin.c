#include <lua.h>
#include <lauxlib.h>

#if LUA_VERSION_NUM < 502
	#define luaL_newlib(L,l) (lua_newtable(L), luaL_register(L,NULL,l))
#endif

LUALIB_API int sadd (lua_State *L) {
	size_t l1, l2, l3;
	const char *s1 = luaL_checklstring(L, 1, &l1);
	lua_Integer p1 = luaL_checkinteger(L, 2);
	const char *s2 = luaL_checklstring(L, 3, &l2);
	lua_Integer p2 = luaL_checkinteger(L, 4);
	lua_Integer length = luaL_checkinteger(L, 5);
	lua_Integer outIndex = luaL_checkinteger(L, 7);
	luaL_Buffer buff;
	luaL_checktype(L, 6, LUA_TTABLE);

	if (p1 < 1 || p1 > l1 || p2 < 0 || (p2 > 0 && p2 > l2)) luaL_error(L, "invalid starting indices");
	if (p1+length-1 > l1) luaL_error(L, "source string is insufficiently long");
	if (length < 0) luaL_error(L, "expected a non-negative length");
	if (length == 0) {
		lua_pushstring(L, "");
		return 1;
	}

	s1 += p1-1, s2 += p2-1;
	l1 -= p1-1, l2 -= p2-1;
	if (l2 < length) {
		l3 = length - l2;
		length = l2;
	} else {
		l3 = 0;
	}
	
	luaL_buffinit(L, &buff);
	while (length --> 0)
		luaL_addchar(&buff, *s1++ + *s2++);
	while (l3 --> 0)
		luaL_addchar(&buff, *s1++);
	
	luaL_pushresult(&buff);
	lua_settable(L, 6);
	lua_pushinteger(L, outIndex+1);
	
	return 1;
}

static struct luaL_Reg binlib[] = {
	{"sadd", sadd},
	{NULL, NULL}
};

int luaopen_casc_binc (lua_State *L) {
	luaL_newlib(L, binlib);
	return 1;
}