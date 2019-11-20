assert(type(nil) == 'nil')
assert(type(4) == 'number')
assert(type('hi') == 'string')
assert(type(type) == 'function')
assert(type({}) == 'table')

local s = type('hi') .. type(4) .. type(nil)
assert(s == 'stringnumbernil')
