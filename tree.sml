type key = string
datatype 'a tree = LEAF
                 | TREE of 'a tree * key * 'a * 'a tree

val empty = LEAF

fun insert (key, v, LEAF) = TREE(LEAF, key, v, LEAF)
  | insert (key, v', TREE(l, k, v, r)) =
    if key < k
    then TREE(insert(key, v', l), k, v, r)
    else if key > k
    then TREE(l, k, v, insert(key, v', r))
    else TREE(l, key, v', r)

infix 2 or
fun false or false = false
  | _ or _ = true

fun member (LEAF,  key) = false
  | member (TREE(l, k, _, r), key) =
    if key = k
    then true
    else member (l, key) or member (r, key)

infix 2 or'
fun NONE or' NONE = NONE
  | NONE or' (SOME v) = SOME v
  | (SOME v) or' NONE = SOME v
  | v or' _  = v

fun lookup (LEAF, key) = NONE
  | lookup (TREE(l, k, v, r), key) =
    if key = k
    then SOME v
    else lookup (l, key) or' lookup (r, key)
