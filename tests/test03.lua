local a = 1 or 2
assert(a == 1)

local b = nil or 3
assert(b == 3)

local c = 4 and 5
assert(c == 5)

local d = nil and 6
assert(d == nil)
