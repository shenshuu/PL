signature Utils =
sig
    val string_multiply : string * int -> string
end

structure Utils =
struct
fun string_multiply (s, n) =
    if n <= 0 then "" else s ^ string_multiply(s, n-1)
end
