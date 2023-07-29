(* 
###################################################################################
Your name:Liam Coyle

I affirm that I have not violated the
Academic Integrity policies detailed in the syllabus
###################################################################################
*)

(* SML comments appear like this *)

(* #1 - pow *)
(* If b = 0 then return 1, else repeat a * a until b = 0*)
fun pow (a, b) = if b = 0 then 1
    else a * pow(a, b - 1);
	

(* #2 - sumTo *)
(* If x = 0 then return 0, else starting at the lowest value reciprocal add 1/x repeatedly subtracting 1 from x until x = 0*)
fun sumTo (x:int) = if x = 0 then 0.0 else 1.0/real(x) + sumTo(x - 1);
               
               
(* #3 - repeat *)
(*
If repeat number is zero return nothing
else concatenate s to the string and resend the string and count number number - 1 into the function recursivly until zero.
*)
fun repeat (s, n) = if n = 0 then ""
    else s ^ repeat(s, n - 1);

(* #4 - binary *)
(*
works until b equals zero
divFlat() calculates what x divided by 2 is without carrying into decimal.
binaryHelper() continuously iterates through the counter until zero, dividing the number in half 
    and adding either 1 or 0 to the left of the string each time there is a remainder of x mod 2
binary() calls binaryHelper with the length the binary number should be
*)

fun divFlat (x) = if x mod 2 = 1 then (x-1) div 2 else 0; 
fun binaryHelper (x, b) = if b = 0 then "" else if x mod 2 = 1 then binaryHelper(divFlat(x), b-1) ^ "1" else binaryHelper(x div 2, b-1) ^ "0"; 
fun binary x = binaryHelper(x, 16);

(* #5 - countNegative *)
(*
works until list is empty
If null then countNegative x = 0
If the number is negative increment the counter and resend the tail of the list into the function recursively
*)
fun countNegative x = if null x then 0 else if hd x < ~0 then 1 + countNegative(tl x) else 0 + countNegative(tl x);

(* #6 - absList *)
(*
absList() Iterates through listen until the end and sends the head of the list to absHelper()
absHelper() returns the absolute values of the numbers in the tuple.
*)

fun absHelper (x: int * int) = [(abs(#1 x), abs(#2 x))];
fun absList x = if null x then [] else absHelper(hd x) @ absList(tl x);

(* #7 - split *)
(*
split() Iterates through listen until the end and sends the head of the list to splitHelper()
splitHelper() If the value is even split it and return both halves in a tuple, if the number is odd,
 modulo divide and carry the one into the right side of the tuple.
*)
fun splitHelper (x) = if x mod 2 = 0 then [(x div 2, x div 2)] else [(x div 2, (x div 2) + 1)];
fun split x = if null x then [] else splitHelper(hd x) @ split(tl x);


(* #8 - isSorted *)
(*
isSorted() runs until the list is empty or is stopped by an out of order value
isSortedHelper() recursively compares the beginning value to the next value, then replaces the beginning value if it is smaller and 
compares to the next value in the list continuously
 *)
fun isSortedHelper (x, y) = if null y then true else if x <= hd y then isSortedHelper(hd y, tl y) else false;
fun isSorted x = if null x then true else isSortedHelper(hd x, tl x);

 
(* #9 - collapse *) 

(*
length() finds length of list by continously feeding the list tail into itself until it is null.

collapse() recursively lowers the size of the list by 2 as it collapses until its length is 1 or 0;

collapseHelper() adds the first two numbers of a list together and returns the only number if there is just one value to add.
*)

fun length x = if null x then 0 else 1 + length (tl x);(*THIS IS BORROWED FROM THE TEXTBOOK*)

fun collapseHelper (x) = if null x then [] else if length(x) <= 1 then [hd x] else [hd x + hd(tl x)];

fun collapse x = if null x then [] else if length(x) <= 1 then x else collapseHelper(x) @ collapse(tl(tl x));

        
(* #10 - insert *)        
(*
Remove list values that are lower than the number then add number to front of remaining list items and recombine. 
If the list is empty or becomes empty jsut add the number.
*)

fun insert (n, x) = if null x then [n] else if n > hd x then [hd x] @ insert(n, tl x) else [n] @ x;


(* #11 - decimal *)
(*
reverse() Reverses a list by recursively adding the tail infront of the head.
decimalHelper() Reads a backwards binary number and recursively feeds its own tail to add the
    power of each position together if its value is equal to 1. 
decimal() Explodes the string into a list, reverses the list, and adds a counter for decimalHelper()
*)


fun reverse L = if null L then nil else reverse(tl L) @ [hd L]; (*THIS IS BORROWED FROM THE TEXTBOOK*)
fun decimalHelper(n , x) = if null n then 0 else if hd n = #"1" then decimalHelper(tl n, x + 1) + pow(2, x) else decimalHelper(tl n, x + 1);
fun decimal (s:string) = decimalHelper(reverse(explode(s)) , 0);
