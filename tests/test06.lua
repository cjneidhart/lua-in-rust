local foo = function ()
  -- Do nothing
end

local should_be_nil = foo()
assert(should_be_nil == nil)
