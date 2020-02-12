-- Stress-test the GC

local tbl = {}

local say_hi = function ()
  return 'hello'
end

local count, step = 100000, 1000
for i = 1, count do
  local t1 = {}
  local t2 = {}
  t1.x = t2
  t2.y = t1
  if i % step == 0 then
    tbl[i] = t1
  end
end
for i = step, count, step do
  assert(type(tbl[i]) == 'table')
  assert(type(tbl[i].x) == 'table')
  assert(tbl[i] == tbl[i].x.y)
end

assert(say_hi() == 'hel' .. 'lo')

-- TODO: run the gc now, then check its size
