-- Test table constructors

local table1 = { name = 'Chris', id = '42' }
assert(table1.name == 'Chris')
assert(table1.id == '42')

local table2 = {
  [6 * 7] = 'forty-two';
  ['hello' .. ' ' .. 'world'] = 'hi';
}
assert(table2[42] == 'forty-two')
assert(table2['hello world'] == 'hi')
