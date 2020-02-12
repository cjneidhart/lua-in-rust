-- Test some less-common standard library functions
-- (but not any of the functions in separate modules like `os`, `string`, etc.)

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
