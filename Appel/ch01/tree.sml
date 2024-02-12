type key = string
datatype 'a tree = LEAF | TREE of 'a tree * 'a * 'a tree

val empty = LEAF

fun insert (key,LEAF) = TREE(LEAF,key,LEAF)
  | insert(key,TREE(l,k,r)) =
    if key < k
    then TREE(insert(key,l),k,r)
    else if key > k
    then TREE(l,k,insert(key,r))
    else TREE(l,key,r)

fun member (key,LEAF) = false
  | member(key,TREE(l,k,r)) =
	       if key < k
	       then member(key,l)
	       else if key > k
	       then member(key,r)
	       else true
			
val my_tree = TREE(TREE(LEAF,"1",LEAF),"2",LEAF)
