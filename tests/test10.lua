-- Test some less-common standard library functions
-- (but not any of the functions in separate modules like `os`, `string`, etc.)

-- unpack

local tbl = {'x', 'y', 'z'}
x, y, z = unpack(tbl)
assert(x == 'x')
assert(y == 'y')
assert(z == 'z')

local x, y, z = unpack(tbl)
assert(x == 'x')
assert(y == 'y')
assert(z == 'z')

local x, y, z, a, b = unpack(tbl)
assert(x == 'x')
assert(y == 'y')
assert(z == 'z')
assert(a == nil)
assert(b == nil)

-- ipairs
-- TODO: rewrite this using for loops
local tbl = {'a', 'b', 'c'}
local f, t, n = ipairs(tbl)
assert(t == tbl)
assert(n == 0)
local key, value = f(t, n)
assert(key == 1)
assert(value == 'a')
local key, value = f(t, key)
assert(key == 2)
assert(value == 'b')
local key, value = f(t, key)
assert(key == 3)
assert(value == 'c')
local key, value = f(t, key)
assert(key == nil and value == nil)
