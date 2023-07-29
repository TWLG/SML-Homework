(* 
###################################################################################
Your name: Liam Coyle
 
I affirm that I have not violated the
Academic Integrity policies detailed in the syllabus
###################################################################################
*)

(* #1 - duplist *)
(*duplicates every item in a list, doubling the size of the list while keeping the order, such as [1,2] would turn into [1,1,2,2]*)
fun duplist x = foldr (fn (x, acc) => x :: x :: acc) [] x;

(* #2 - mylength *)
(*finds the length of the list by setting all the values in the list to 1 and adding them together*)
fun mylength x = foldr (op +) 0 (map (fn x => 1) x);

(* #3 - il2absrl *)
(*returns the absolute real numbers of all value in a list*)
fun il2absrl x = map (fn (x) => real(abs(x))) x;

(* #4 - myimplode *)
(*concatenates a list of characters into a string*)
fun myimplode x = foldr (fn (x, acc) => str(x) ^ acc) "" x;

(* #5 - lconcat *)
(*concatenates a list of sublists into a list with no sublists*)
fun lconcat x = foldr (fn (x, acc) => x @ acc) [] x;

(* #6 - convert *)

(*supposed to convert a list of tuples into a tuple of lists where items in slot 0 of a tuple is set to list 0 and slot 1 is set to list 1*)
fun convert x =(*
    let
      val folder = fn ((a, b), (aList, bList)) => (a :: aList, b :: bList)
    in
      foldl (fn (pair, acc) => folder (acc, pair)) ([], []) x
    end*)(nil,nil);

(* #7 - mymap *)

(*repeats the functionality of map without using the map function*)
fun mymap f x = foldr (fn (x, acc) => f x :: acc) [] x;

(* #8 - myfoldl *)
fun myfoldl f initialValue aList = ~1;

(* #9 - sumSome *)
(* sumSome takes a predicate function and returns a function that sums elements in a list satisfying the predicate *)
fun sumSome (f:int -> bool) = 
  let
    fun sumHelper [] = 0
      | sumHelper (x :: xs) = if f x then x + sumHelper xs else sumHelper xs
  in
    sumHelper
  end;