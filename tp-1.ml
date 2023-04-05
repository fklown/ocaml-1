(* Abstraction layers *)
let increment i = i + 1
let are_empty l l2 = l = [] && l2 = []
let duplicate_entry l = l @ l
let empty_list = []
let head_of l = List.hd l
let head_length_of l = List.length (head_of l)
let is_even i = i mod 2 = 0
let is_odd i = i mod 2 = 1
let length_of l = List.length l
let has_index l = length_of l >= 1
let tail_of l = List.tl l
let first_index = 1

(* remove_last *)
let remove_last l =
  let rec map_only_last_index input result index input_length =
    if are_empty input result then empty_list
    else if input_length > index then
      map_only_last_index (tail_of input)
        (result @ [ head_of input ])
        (increment index) input_length
    else result
  in
  map_only_last_index l empty_list first_index (length_of l)

(* listes_paires *)
let listes_paires l =
  let rec double_indexes_with_odd_length input result =
    if are_empty input result then empty_list
    else if has_index input && is_even (head_length_of input) then
      double_indexes_with_odd_length (tail_of input) (result @ [ head_of input ])
    else if has_index input && is_odd (head_length_of input) then
      double_indexes_with_odd_length (tail_of input)
        (result @ [ duplicate_entry (head_of input) ])
    else result
  in
  double_indexes_with_odd_length l empty_list

(* remove_half *)
let remove_half l =
  let rec map_odd_indexes input result index =
    if are_empty input result then empty_list
    else if has_index input && is_even index then
      map_odd_indexes (tail_of input) result (increment index)
    else if has_index input && is_odd index then
      map_odd_indexes (tail_of input)
        (result @ [ head_of input ])
        (increment index)
    else result
  in
  map_odd_indexes l empty_list first_index

(*
   ============ TESTS ============
*)
(* remove_last should only remove the last element of a list *)
let expected = [ "titi"; "toto" ]
let actual = remove_last [ "titi"; "toto"; "tata" ];;

let assertion =
  if expected = actual then "Use case has been VERIFIED for remove_last"
  else "Use case has FAILED for remove_last"
in
print_endline assertion

(* listes_paires should only duplicate list elements with odd length *)
let expected =
  [
    [];
    [ 1; 1 ];
    [ 1; 2 ];
    [ 1; 2; 3; 1; 2; 3 ];
    [];
    [ 5; 4; 3; 2; 1; 5; 4; 3; 2; 1 ];
  ]

let actual =
  listes_paires [ []; [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ]; []; [ 5; 4; 3; 2; 1 ] ]
;;

let assertion =
  if expected = actual then "Use case has been VERIFIED for listes_paires"
  else "Use case has FAILED for listes_paires"
in
print_endline assertion

(* remove_half should return a list containing only odd index elements *)
let expected = [ 'a'; 'c'; 'e' ]
let actual = remove_half [ 'a'; 'b'; 'c'; 'd'; 'e' ];;

let assertion =
  if expected = actual then "Use case has been VERIFIED for remove_half"
  else "Use case has FAILED for remove_half"
in
print_endline assertion

(*
   ============ END of TESTS ============
*)