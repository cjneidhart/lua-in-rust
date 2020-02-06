function return_five()
  return 5
end

assert(return_five() == 5)

do
  local my_local
  function my_local()
    return 'hello'
  end
  my_global = my_local
end

assert(my_local == nil)
assert(my_global() == 'hello')
