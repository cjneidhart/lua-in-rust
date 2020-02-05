add = function (x, y)
  local sum = x + y
  return sum
end

identity = function () end

assert(add(2, 7) == 9)
assert(add(5, 11, 41) == 16)
assert(identity() == nil)
