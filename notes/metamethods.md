A list of all the metamethods used by the standard library:

### Arithmetic
Both values must be numbers (or strings coercible to numbers). Otherwise,
Lua will try a metamethod, prioritizing the left value.
- `__add`: `+`
- `__sub`: `-`
- `__mul`: `*`
- `__div`: `/`
- `__mod`: `%`
- `__pow`: `^`
- `__idiv`: `//`
- `__unm`: Unary `-`

### Bitwise operations
Same as normal arithmetic, except the values must be integers, floats coercible
to integers, or strings coercible to integers.
- `__band`: `&`
- `__bor`: `|`
- `__bxor`: `~` (binary XOR)
- `__bnot`: Unary `~`
- `__shl`: `<<`
- `__shr`: `>>`

### Equality
- `__eq`: Called by `==` and `~=`, only if the values are both tables or both
  userdata, and they are not primitively equal.

### Comparisons
It's easier to list the operators. These are not called if both values are
strings or both are numbers (strings will be compared lexographically).
- `<`: Looks for `__lt`.
- `>`: Same as `__lt`, but swaps the arguments.
- `<=`: First looks for `__le` in both. Then, proceeds as if it were
  `not (rhs < lhs)`.
- `>=`: Same as `<=`, but swaps the arguments.

### Indexing
- `__index`: Called by `table[key]` when `table` is not a table or `key` is not
  in `table`. Can be another table instead of a function, in which case it will
  index that table (which can trigger another metamethod).
- `__newindex`: Called by `table[key] = value`, same as `__index`.

### Other
- `__concat`: `..`. Only called if one value is not a string or number (numbers
  will be coerced to strings).
- `__len`: `#`. Only called if the value is not a string. If there's no
  metamethod and it's a table, then it will perform the normal `#` operation.
- `__call`: Called by `func(args...)` when `func` is not a function. The
  metamethod will receive `func` as its first argument, followed by the values
  in `args...`.

### Mentioned elsewhere
- `__gc`: Must be in the metatable before it is set as a metatable. The object
  must be a table or userdata. Calls the `__gc` metamethod when the object
  is garbage-collected.
- `__mode`: Should be a string, not a function. If a table's metatable has
  field `__mode` and that string contains `'k'`, it has weak keys. If the
  string contains `v`, it has weak values.
- `__metatable`: Used by `getmetatable` and `setmetatable` in the standard
  library (but not the equivalent functions in the C/Rust API). If this field
  is set, `getmetatable` will return this value instead of the actual
  metatable, and `setmetatable` will raise an error.
- `__name`: Used by the registry system.
- `__pairs`: Called by the `pairs` function, if it exists. Otherwise, pairs
  returns `next`, the table, and `nil`.
- `__tostring`: Used by the `tostring` function.
