vars
    p1 p2 p3

rules
    p1 >= 1 ->
	       p1' = p1-1,
	       p2' = p2+1;
    p2 >= 1 ->
 	       p2' = p2-1,
 	       p3' = p3+1;

init
    p1=2, p2=0, p3=0
target
    p3 >= 2
