; this doesn't work because tail recursion assumes that 
; the binary operation you're applying to the data is 
; associative. Changing the order of cons won't work
; either because now you're repeating sublists