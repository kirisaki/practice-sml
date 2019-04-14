datatype expr =
         ADD of expr * expr
       | MUL of expr * expr
       | NUM of int

fun eval (NUM x) = NUM x
  | eval (ADD (NUM x, NUM y)) = NUM (x + y)
  | eval (ADD (x, y))  = eval (ADD (eval x, eval y))
  | eval (MUL (NUM x, NUM y)) = NUM (x * y)
  | eval (MUL (x, y))  = eval (MUL (eval x, eval y))

val foo = ADD (NUM 1
              ,MUL (NUM 2
                   ,NUM 3
                   )
              )

exception Unextractable

fun extract (NUM x) = x
  | extract _ = raise Unextractable

fun main () = print (Int.toString (extract (eval foo)) ^ "\n\n")
