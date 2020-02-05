local return_nil = function ()
  -- Do nothing
end

local return_5 = function ()
  return 5
end

local should_be_nil = return_nil()
local should_be_5 = return_5()

assert(should_be_nil == nil)
assert(should_be_5 == 5)
