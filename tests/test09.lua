-- Test table constructors

-- Basic field=expr style
local table1 = { name = 'Chris', id = '42' }
assert(table1.name == 'Chris')
assert(table1.id == '42')

-- Square brackets allow arbitrary expressions as keys
local table2 = {
  [6 * 7] = 'forty-two';
  ['hello' .. ' ' .. 'world'] = 'hi';
}
assert(table2[42] == 'forty-two')
assert(table2['hello world'] == 'hi')

-- Array-style
local table3 = {
  'one', 'two', 'three'
}
assert(table3[1] == 'one')
assert(table3[2] == 'two')
assert(table3[3] == 'three')

-- Mixed style
local table4 = {
  'one',
  name = 'chris',
  'two',
  [10 * 10 + 11] = 'eleventy-one',
  'three',
}
assert(table4[1] == 'one')
assert(table4.name == 'chris')
assert(table4[2] == 'two')
assert(table4[111] == 'eleventy-one')
assert(table4[3] == 'three')
