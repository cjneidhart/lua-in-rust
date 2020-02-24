## Weak tables
Changing the `__mode` field of a table after it has become a metatable is
undefined behavior:
```lua
t, meta = {}, {}
setmetatable(t, meta)
meta.__mode = 'kv' -- BAD!
```

## Modification during traversal
The standard library function `next` by default is guaranteed to visit every
element exactly once. If you assign to an empty field in the table
mid-traversal, and then try to continue from the same key, its behavior is
undefined. Note that it is perfectly fine to modify existing fields, and to
clear them by assigning `nil`.

## The Length Operator
If a table has `holes` among its numeric keys, the length operator `#` may
return any of the holes as its length.
```lua
t = { 'a', 'b', 'c' }
t[2] = nil
print(#t) -- Could be 1 or 3
```

## Invalid table keys
This is not in the reference, but `nil` and any numbers which are NaN (i.e.
`a != a`) are disallowed as table keys. When reading from a table, using these
keys will always return `nil`. When assigning to a table, using these keys will
cause an error.
