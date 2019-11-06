-- Test nesting of local variables

local l0 = 'a'
assert(l0 == 'a')

do
  local l0 = 'b'
  local l1 = 'c'
  assert(l0 == 'b')
  assert(l1 == 'c')
end
assert(l0 == 'a')
assert(l1 == nil)

local cond = true
while cond do
  assert(l1 == nil)
  local l0 = 'd'
  local l2 = 'e'
  assert(l0 == 'd')
  assert(l2 == 'e')
  cond = false
end
assert(l0 == 'a')
assert(l2 == nil)

repeat
  local l0 = 'f'
  local l3 = 'g'
  assert(l0 == 'f')
  assert(l3 == 'g')
until true
assert(l0 == 'a')
assert(l3 == nil)

for i = 1, 10 do
  local l0 = 'h'
  local l3 = 'i'
  assert(l0 == 'h')
  assert(l3 == 'i')
end
assert(l0 == 'a')
assert(l1 == nil)
