(* Sum of a list *)
fun sumList [] = 0
| sumList (h::t) = h + sumList t;


(* Fibonacci *)
fun fibonacci 0 = 0                                                                
| fibonacci 1 = 1                                                                 
| fibonacci n = fibonacci (n-1) + fibonacci (n-2);


(* Reverse *)
fun reverse(L) =
   if L = nil then nil
   else reverse(tl(L)) @ [hd(L)];




(* Rotate *)
fun rotate(L,N)  =
  let
    val n = abs(length(L) - N)
  in
    (List.drop(L, n) @ List.take(L, n))
  end;
  


 (* Merge Sort *)
fun take(L) =
 if L = nil then nil
 else hd(L)::skip(tl(L))
and skip(L) =
 if L=nil then nil
    else take(tl(L));

fun merge([],M) = M
| merge(L,[]) = L
| merge(x::xl,y::yl) =
    if (x:int)<y then x::merge(xl,y::yl)
    else y::merge(x::xl,yl);

fun msort(L) =
 if L= [] then []
 else if tl(L)=[] then L
 else merge(msort(take(L)),msort(skip(L)));


(* Tower of hanoi *)
fun hanoi(0, a, b, c) = []
|hanoi(1,a,b,c) = [(a,c)]
|hanoi(n, a, b, c) =
    hanoi(n-1, a, c, b) @ hanoi(1,a,b,c) @ hanoi(n-1, b, a,c);
       

