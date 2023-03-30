val test1a = alternate([1,2,3,4]) = ~2
val test1b = alternate([]) = 0
val test1c = alternate([1,3,2]) = 0

val test2a = min_max([1,2,3,4]) = (1,4)
val test2b = min_max([3,4,1]) = (1,4)
val test2c = min_max([5,3,11]) = (3,11)

val test3a = cumsum([1,4,20]) = [1,5,25]
val test3b = cumsum([1,3,7]) = [1,4,11]

val test4a = greeting(NONE) = "Hello there, you!"
val test4b = greeting(SOME("Mike")) = "Hello there, Mike!"