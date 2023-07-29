(* 
###################################################################################
Your name: Liam Coyle

I affirm that I have not violated the
Academic Integrity policies detailed in the syllabus
###################################################################################
*)

(* #1 - quicksort 
picks a pivot point and splits the list into sublists, seperating the values that are greater or less 
than the pivot point recursively then rejoining the lists. 
*)

fun quicksort [] = []
  | quicksort (x::xs) =
    let
      fun partition (p, [], left, right) = (left, right)
        | partition (p, y::ys, left, right) =
          if y < p then partition (p, ys, y::left, right)
          else partition (p, ys, left, y::right)
      val (left, right) = partition (x, xs, [], [])
    in
      quicksort left @ [x] @ quicksort right
    end

(* #2 - member 
iterates as and searches the list starting from the head comparing each value to the prompted value.
*)
fun member (e, s) = if length(s) < 1 then false else if e = hd s then true else member(e, tl s);
               
               
(* #3 - returns the union of sets (lists) s1 and s2*)
(* You may assume that s1 and s2 do not have any duplicate elements

adds unique values to s2 and returns s2 as the union of the two lists.
 *)


fun union (s1, s2) = if length(s1) < 1 then quicksort(s2) else 
    if length(s2) < 1 then quicksort(s1) else 
    if member (hd s1, s2) then union(tl s1, s2) else union(tl s1, [hd s1] @ s2);

(* #4 - returns the intersection of sets (lists) s1 and s2 *)
(* You may assume that s1 and s2 do not have any duplicate elements 

Compares each value of s1 to s2, creating a list and returning all matches
*)
fun intersection (s1, s2) = if length(s1) < 1 then [] else 
    if length(s2) < 1 then [] else if member (hd s1, s2) then hd s1 :: intersection(tl s1, s2) else intersection(tl s1, s2);

(* #5 - Return list of integers from start (inclusive) to stop (exclusive) by step 
adds integers to the list until they are >= the stop value.
special case for ranges counting down where integers will decrease in value until <= stop
*)
fun range(start, stop, step) = if step > 0 then if (start + step) < stop then start :: range((start + step), stop, step) else [start] else 
    if (start + step) > stop then start :: range((start + step), stop, step) else [start];

(* #6 - Return a slice of a list between indices start inclusive, and stop exclusive. Assume first element of list is at index 0*)
fun nthElement(x, aList) = if x < 1 then hd aList else nthElement(x-1, tl aList);
fun slice(aList, start, stop) = if start < stop then nthElement(start, aList) :: slice(aList, start + 1, stop) else [];
 
(* #7 - binary search*)

(*mid() - finds the middle value of the list, if the list is odd then return 
the right of the middle two values RETURNS INT NOT INT LIST*)
fun mid(aList) =
  let
    val len = length aList
  in
    if len < 2 then hd aList
    else if len mod 2 = 0 then nthElement(len div 2, aList)
    else nthElement(len div 2, aList)
  end  

(*leftHalf()/rightHalf() - for clarity these two functions respectively return the left or right sides of the list seperately from the search function
however the right side also includes the middle value*)
fun leftHalf(aList) = slice(aList, 0, length aList div 2);
fun rightHalf(aList) = slice(aList, (length aList div 2), length aList);

(* binarySearch() - if the list is null then false, if the list is of length 1 but makes it past the mid() check is it false,
if the value is greater or less than mid() send the respective sliced half of the list into another binary search until
the value has been compared through*)
fun binarySearch(sortedList, value) =
    if length sortedList < 1 then false else 
    if mid(sortedList) = value then true else
    if length sortedList = 1 then false else
    if mid(sortedList) > value then binarySearch(leftHalf(sortedList), value) else binarySearch(rightHalf(sortedList), value);

 