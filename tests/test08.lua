-- Test function declarations.

-- A basic function declaration
function return_five()
  return 5
end
assert(return_five() == 5)

do
  local my_local
  -- A basic function declaration, which assigns to a local instead
  function my_local()
    return 'hello'
  end
  assert(my_local() == 'hello')
  my_global = my_local
end
assert(my_local == nil)
assert(my_global() == 'hello')

-- A table function declaration
tbl = {}
function tbl.return_forty_two()
  return 42
end
assert(tbl.return_forty_two() == 42)

