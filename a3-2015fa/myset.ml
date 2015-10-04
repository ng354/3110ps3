(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  open Order
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false

  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)

  let fold f e = List.fold_left (fun a x -> f x a) e

  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(struct
    type key = C.t
    type value = unit
    let compare = C.compare
    let string_of_key = C.string_of_t
    let string_of_value _ = ""

    (* These functions are for testing purposes *)
    let gen_key () = C.gen ()
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = ()
    let gen_pair () = (gen_key (), gen_value ())
  end)

  type elt = D.key
  type set = D.dict

  (* returns empty Dictset *)
  let empty = D.empty

  (* Checks if the set s is empty *)
  let is_empty s = s=empty

  (* inserts element k into set s *)
  let insert k s = D.insert s k ()

  (* same as insert x empty *)
  let singleton k = D.insert empty k ()

  (* returns a new set of the union of sets s1 and s2 *)
  let union s1 s2 =
    D.fold (fun k () d -> insert k d) s1 s2

  (* returns a new set of the intersection of sets s1 and s3 *)
  let intersect s1 s2 =
    D.fold (fun k () d -> if D.member s2 k then insert k d else d) empty s1

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  let remove k s =
    D.remove s k

  (* returns true iff the element is in the set *)
  let member s k =
    D.member s k

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  let choose s =
    match D.choose s with
    | None -> None
    | Some(k,v,s1) -> Some(k,s1)

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  let fold f a s =
    D.fold (fun elem () acc -> f elem acc) a s

  (* functions to convert our types to a string. useful for debugging. *)
  let string_of_set s =
    "Set: "^(D.string_of_dict s)

  let string_of_elt k =
    D.string_of_key k

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* Test if the empty dict set is an empty dictionary *)
  let test_empty () =
    let dict_set_empty = empty in
    assert(dict_set_empty=D.empty);
    ()

  (* Test if is_empty will return true for an empty dictset *)
  let test_is_empty () =
    let dict_set_empty = empty in
    assert(is_empty dict_set_empty);
    ()

  (* Test if is_empty wil return false for a non-empty dictset *)
  let test_is_empty_false () =
    let elt = C.gen_random () in
    let dict_set_not_empty = insert elt empty in
    assert(not (is_empty dict_set_not_empty));
    ()

  (* Test insertion into empty set *)
  let test_insert () =
    let elt = C.gen_random () in
    let s1 = insert elt empty in
    assert(member s1 elt);
    ()

  (* Test insertion into non-empty set *)
  let test_insert_2 () =
    let elt = C.gen_random () in
    let s1 = insert elt empty in
    let elt2 = C.gen_random () in
    let new_s1 = insert elt2 s1 in
    assert(member s1 elt);
    assert(member new_s1 elt);
    ()

  (* Test singleton set membership *)
  let test_singleton_elt () =
    let elt = C.gen_random () in
    let single_set = singleton elt in
    assert(not (is_empty single_set));
    assert(member single_set elt);
    ()

  (* Test singleton set size = 1 *)
  let test_singleton_size_1 () =
    let elt = C.gen_random () in
    let single_set = singleton elt in
    assert(not (is_empty single_set));
    let empty_single_set = remove elt single_set in
    assert(is_empty empty_single_set);
    ()

  (* Test union. s1 has 2 elets, s2 has 1. s should contain all 3 *)
  let test_union_1 () =
    let elt1 = C.gen_random () in
    let start_s1 = singleton elt1 in
    let elt2 = C.gen_random () in
    let s1 = insert elt2 start_s1 in
    let elt3 = C.gen_random () in
    let s2 = singleton elt3 in
    let s = union s1 s2 in
    assert(member s elt1);
    assert(member s elt2);
    assert(member s elt3);
    ()

  (* Tests if one set is empty and other is singleton. s is singleton *)
  let test_union_2 () =
    let elt1 = C.gen_random () in
    let s1 = singleton elt1 in
    let s2 = empty in
    let s = union s1 s2 in
    assert(member s elt1);
    let new_s = remove elt1 s in
    assert(is_empty new_s);
    ()

  (* Tests if both sets are empty *)
  let test_union_3 () =
    let s1 = empty in
    let s2 = empty in
    let s = union s1 s2 in
    assert(is_empty s);
    ()

  (* Tests if both sets have the same element if no duplicates *)
  let test_union_4 () =
    let elt = C.gen_random () in
    let start_s1 = singleton elt in
    let elt2 = C.gen_random () in
    let s1 = insert elt2 start_s1 in
    let s2 = singleton elt in
    let s = union s1 s2 in
    assert(member s elt);
    assert(member s elt2);
    let new_s = (remove elt (remove elt2 s)) in
    assert(is_empty new_s);
    ()

  (* Test intersect of two sets with 2 elts each, sharing 1 elt *)
  let test_intersect_1 () =
    let elt = C.gen_random () in
    let elt2 = C.gen_random () in
    let elt3 = C.gen_random () in
    let s1 = insert elt (singleton elt2) in
    let s2 = insert elt (singleton elt3) in
    let s = intersect s1 s2 in
    assert(member s elt);
    assert(not (member s elt2));
    assert(not (member s elt3));
    ()

  (* Test intersect of two sets with no shared elts *)
  let test_intersect_2 () =
    let s1 = singleton (C.gen_random ()) in
    let s2 = singleton (C.gen_random ()) in
    let s = intersect s1 s2 in
    assert(is_empty s);
    ()

  (* Test intersect of two sets, one empty *)
  let test_intersect_3 () =
    let s1 = singleton (C.gen_random ()) in
    let s2 = empty in
    let s = intersect s1 s2 in
    assert(is_empty s);
    ()

  (* Test intersect of two empty sets *)
  let test_intersect_4 () =
    let s1 = empty in
    let s2 = empty in
    let s = intersect s1 s2 in
    assert(is_empty s);
    ()

  (* Test removal of a two element set *)
  let test_remove_1 () =
    let elt1 = C.gen_random () in
    let elt2 = C.gen_random () in
    let s = insert elt1 (singleton elt2) in
    let new_s = remove elt1 s in
    assert(member new_s elt2);
    assert(not(member new_s elt1));
    ()

  (* Test removal of a one element set *)
  let test_remove_2 () =
    let elt = C.gen_random () in
    let s = singleton elt in
    let new_s = remove elt s in
    assert(is_empty new_s);
    assert(not(member new_s elt));
    ()

  (* Test removal of element not in set *)
  let test_remove_3 () =
    let s = singleton (C.gen_random ()) in
    let new_s = remove (C.gen_random ()) s in
    assert(not (is_empty new_s));
    ()

  (* Test membership true *)
  let test_member_true () =
    let elt = C.gen_random () in
    let s = singleton elt in
    assert(member s elt);
    ()

  (* Test membership false *)
  let test_member_false () =
    let elt = C.gen_random () in
    let s = singleton (C.gen_random ()) in
    assert(not(member s elt));
    ()

  (* gets the value out of an option *)
  let test_get_opt opt =
    match opt with
    | None -> raise (Invalid_argument "Option.get")
    | Some (k,s) -> (k,s)

  (* Test choose of two element set *)
  let test_choose_1 () =
    let elt = C.gen_random () in
    let elt1 = C.gen_random () in
    let s = insert elt1 (singleton elt) in
    let chosen_opt = choose s in
    let (chosen_elt, new_s) = test_get_opt chosen_opt in
    assert(not (member new_s chosen_elt));
    assert(not (is_empty new_s));
    ()

  (* Test choose of one element set *)
  let test_choose_2 () =
    let elt = C.gen_random () in
    let s = singleton elt in
    let chosen_opt = choose s in
    let (chosen_elt, new_s) = test_get_opt chosen_opt in
    assert(not (member new_s elt));
    assert(is_empty new_s);
    ()

  (* Test choose of zero element set, should raise error *)
  let test_choose_3 () =
    let s = empty in
    let chosen_opt = choose s in
    try
      let (chosen_elt, new_s) = test_get_opt chosen_opt in
      assert(is_empty new_s);
      ()
    with
    | Invalid_argument("Option.get") ->
    ()

  (* Test fold one 2 element set, incrementing by 1 both times *)
  let test_fold_1 () =
    let elt1 = C.gen_random () in
    let elt2 = C.gen_random () in
    let s = insert elt2 (singleton elt1) in
    let new_s = fold (fun elmt d -> d+1) 0 s in
    assert(new_s=2);
    ()

  (* Test fold on empty set w/ fxn to duplicate set *)
  let test_fold_2 () =
    let s = empty in
    let new_s = fold (fun elt d -> insert elt d) empty s in
    assert(is_empty new_s);
    ()

  (* Test string_of_set on regular set *)
(*   let test_string_of_set () =
    let s = insert (C.gen_random ()) (singleton (C.gen_random ())) in
    let str = string_of_set s in
    assert(str = "")
    () *)

  (* add your test functions to run_tests *)
  (* Order of tests ensure that later tests which use earlier fxns work *)
  let run_tests () =
    test_empty () ;
    test_is_empty () ;
    test_is_empty_false () ;
    test_insert () ;
    test_insert_2 () ;
    test_member_true () ;
    test_member_false () ;
    test_remove_1 () ;
    test_remove_2 () ;
    test_remove_3 () ;
    test_singleton_elt () ;
    test_singleton_size_1 () ;
    test_union_1 () ;
    test_union_2 () ;
    test_union_3 () ;
    test_union_4 () ;
    test_intersect_1 () ;
    test_intersect_2 () ;
    test_intersect_3 () ;
    test_intersect_4 () ;
    test_choose_1 () ;
    test_choose_2 () ;
    test_choose_3 () ;
    test_fold_1 () ;
    test_fold_2 () ;
    ()
end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable)
let _ = IntListSet.run_tests()

(* Create a set of ints using our DictSet functor
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)

module IntDictSet = DictSet(IntComparable)
let _ = IntDictSet.run_tests()



(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use our dictionary implementation when your are
   * finished. *)
  (* ListSet (C) *)
  DictSet (C)

