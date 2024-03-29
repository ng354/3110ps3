(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)
exception TODO

module type DICT =
sig
  type key
  type value
  type dict

  (* An empty dictionary *)
  val empty : dict

  (* Reduce the dictionary using the provided function f and base case u.
   * Our reducing function f must have the type:
   *      key -> value -> 'a -> 'a
   * and our base case u has type 'a.
   *
   * If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))
   *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* Runs all the tests. *)
  val run_tests : unit -> unit
end



(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Order.order
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Use these functions for testing. *)

  (* Generate a key. The same key is always returned *)
  val gen_key : unit -> key

  (* Generate a random key. *)
  val gen_key_random : unit -> key

  (* Generates a key greater than the argument. *)
  val gen_key_gt : key -> unit -> key

  (* Generates a key less than the argument. *)
  val gen_key_lt : key -> unit -> key

  (* Generates a key between the two arguments. Return None if no such
   * key exists. *)
  val gen_key_between : key -> key -> unit -> key option

  (* Generates a random value. *)
  val gen_value : unit -> value

  (* Generates a random (key,value) pair *)
  val gen_pair : unit -> key * value
end

(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
struct
  open Order
  type key = int
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_key = string_of_int
  let string_of_value v = v
  let gen_key () = 0
  let gen_key_gt x () = x + 1
  let gen_key_lt x () = x - 1
  let gen_key_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
  let num_values = List.length possible_values
  (* gen_value will return the string at this current index *)
  let current_index = ref 0
  let gen_value () =
    let index = !current_index in
    if index >= num_values then
      (current_index := 0; lst_n possible_values index)
    else
      (current_index := index + 1; lst_n possible_values index)
  let gen_pair () = (gen_key_random(), gen_value())
end

(* An association list implementation of our DICT signature. *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
struct
  open Order
  type key = D.key
  type value = D.value
  type dict = (key * value) list

  (* INVARIANT: sorted by key, no duplicates *)

  let empty = []

  let fold f d = List.fold_left (fun a (k,v) -> f k v a) d

  let rec lookup d k =
    match d with
      | [] -> None
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Eq -> Some v1
          | Greater -> lookup d1 k
          | _ -> None)

  let member d k =
    match lookup d k with
      | None -> false
      | Some _ -> true

  let rec insert d k v =
    match d with
      | [] -> [(k,v)]
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Less -> (k,v)::d
          | Eq -> (k,v)::d1
          | Greater -> (k1,v1)::(insert d1 k v))

  let rec remove d k =
    match d with
      | [] -> []
      | (k1,v1)::d1 ->
	(match D.compare k k1 with
          | Eq -> d1
          | Greater -> (k1,v1)::(remove d1 k)
          | _ -> d)

  let choose d =
    match d with
      | [] -> None
      | (k,v)::rest -> Some(k,v,rest)

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string =
    let f = (fun y (k,v) -> y ^ "\n key: " ^ D.string_of_key k ^
      "; value: (" ^ D.string_of_value v ^ ")") in
    List.fold_left f "" d

  (****************************************************************)
  (* Tests for our AssocListDict functor                          *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter (fun (k,v) -> assert(lookup d1 k = Some v)) pairs1 ;
    ()

  let test_remove () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter
      (fun (k,v) ->
        let r = remove d1 k in
        List.iter
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1
      ) pairs1 ;
    ()

  let test_lookup () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter (fun (k,v) -> assert(lookup d1 k = Some v)) pairs1;

    let a =
     match choose d1 with
     |Some (k,v,rest) -> k
     |None -> D.gen_key() in
    let b = remove d1 a in
    assert(lookup b a = None);
    ()

  let test_choose () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    let d2 = empty in

    match choose d2 with
    |None -> assert(true);
    |Some x -> let _ = assert(false) in

    match choose d1 with
    |None -> assert(false);
    |Some (k,v,rest) -> assert(not (member d2 k));
    ()

  let test_member () =
    let pairs1 = generate_pair_list 20 in
    let d1 = insert_list empty pairs1 in
    let a =
     match choose d1 with
     |Some (k,v,rest) -> k
     |None -> D.gen_key() in
    let b = remove d1 a in
    assert(member b a = false);
    ()

  let test_fold () =
    let pairs1 = generate_random_list 20 in
    let d1 = insert_list empty pairs1 in
    let count = fold (fun k v acc -> acc + 1) 0 d1 in
    assert(count = List.length d1);
    ()

  let run_tests () =
    test_insert() ;
    test_remove() ;
    test_lookup() ;
    test_choose() ;
    test_member() ;
    test_fold() ;
    ()

end



(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)
takes in a dict arg and returns a dict
use D.compare for comparison between keys.                        *)
(******************************************************************)

module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  exception TODO

  type key = D.key
  type value = D.value

  (* A dictionary entry is a (key,value) pair. We compare two (key,value)
   * pairs with the provided key-comparison function D.compare. For example,
   * we may choose to keep a dictionary mapping links to their ranks. In this
   * case, our (key,value) pairs will be (link,rank) pairs, and we compare
   * links using string comparison. *)
  type pair = key * value

  (* Type definition for dictionary, which we choose to represent as a 2-3 Tree.
   * This is almost the same as the binary search tree definition from pset4 and
   * lecture, except we add one more case: a Three-node.
   *
   * A Three-node contains two pairs and three subtrees: left, middle, and
   * right, represented by the 3 dicts in the definition below. *)
  type dict =
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* INVARIANTS:
   * 2-node: Two(left,(k1,v1),right)
   * (1) Every key k appearing in subtree left must satisfy k < k1.
   * (2) Every key k appearing in subtree right must satisfy k > k1.
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.
   *
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right)
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must satisfy k < k1.
   * (3) Every key k appearing in subtree right must satisfy k > k2.
   * (4) Every key k appearing in subtree middle must satisfy k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three
   *     subtrees must be the same.
   *)

  (* FOR INSERTION:
   * A kicked configuration returned by going downwards on insertion.
   * We can only kick up Two nodes, hence Up takes a dict * pair * dict *)
  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  (* FOR REMOVAL:
   * A hole configuration returned by going downwards on removal. We
   * include a pair option whenever we remove the minimum of the right
   * subtree of the current pair in order the current pair *)
  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  (* FOR REMOVAL:
   * A direction will distinguish which configuration we came from in the
   * removal cases. We use direction2 for cases (1-2) on the handout, and
   * we use direction3 for cases (3-4) on the handout. *)
  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3

  (* How do we represent an empty dictionary with 2-3 trees? *)
  let empty : dict = Leaf

  (* val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
* If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))*)
  (* TODO:
   * Implement fold. Read the specification in the DICT signature above. *)
  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u
    | Two(Leaf,(k1,v1),Leaf) -> f k1 v1 u
    | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) -> f k1 v1 (f k2 v2 u)
    | Two(l,(k1,v1),r) ->
      let left_u = fold f u l in
      let node_u = f k1 v1 left_u in
      fold f node_u r
    | Three(l,(k1,v1),m,(k2,v2),r) ->
      let left_u = fold f u l in
      let node1_u = f k1 v1 left_u in
      let mid_u = fold f node1_u m in
      let node2_u = f k2 v2 mid_u in
      fold f node2_u r

  (* TODO:
   * Implement these to-string functions *)
  let string_of_key k = D.string_of_key k
  let string_of_value v = D.string_of_value v
  let string_of_dict (d: dict) : string =
    fold (fun k v a -> a^"Key: "^(string_of_key k)^
    ", Value: "^(string_of_value v)^"\n") "Dict: " d

  (* Debugging function. This will print out the tree in text format.
   * Use this function to see the actual structure of your 2-3 tree. *
   *
   * e.g.      (4,d)   (6,f)
   *         /       |       \
   *      (2,b)    (4,d)     Leaf
   *      /  \     /   \
   *   Leaf  Leaf Leaf  Leaf
   *
   * string_of_tree will output:
   * Three(Two(Leaf,(2,b),Leaf),(4,d),Two(Leaf,(5,e),Leaf),(6,f),Leaf)
   *
   * Note that this tree is NOT balanced, because all the paths from (6,f)
   * to its leaves do NOT all have the same length. *)
  let rec string_of_tree (d: dict) : string =
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ ","
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"

  (* Upward phase for w where its parent is a Two node whose (key,value) is x.
   * One of x's children is w, and the other child is x_other. This function
   * should return a kicked-up configuration containing the new tree as a
   * result of performing the upward phase on w. *)
  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (x_other: dict) : kicked =
      match D.compare (fst w) (fst x) with
      | Eq -> Done(Two(w_left,w,w_right))
      | Less ->
        let new_node = Three(w_left, w, w_right, x, x_other) in
        Done new_node
      | Greater ->
        let new_node = Three(x_other, x, w_left, w, w_right) in
        Done new_node

  (* Upward phase for w where its parent is a Three node whose (key,value) is x.
   * One of x's children is w, and of the two remaining children,
   * other_left is the subtree more to the left and other_right is the
   * subtree more to the right.
   *
   * E.g. From our handout, for the first case where w's parent is a Three-tree,
   * other_left would be c and other_right would be d. For the second case,
   * other_left would be a and other_right would be d. For the third case,
   * other_left would be a and other_right would be b.
   *
   * This function should return a kicked-up configuration containing the
   * new tree as a result of performing the upward phase on w. *)
  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
        match (D.compare (fst w) (fst x)), (D.compare (fst w) (fst y)) with
        | Eq,_ -> Done(Three(w_left, w, w_right, y, other_right))
        | _,Eq -> Done(Three(other_left, x, w_left, w, w_right))
        | Less, _ ->
          Up(Two(w_left,w, w_right), x,
                          Two(other_left, y, other_right))
        | Greater, Less ->
          Up(Two(other_left, x, w_left), w,
                          Two(w_right, y, other_right))
        | _, Greater ->
          Up(Two(other_left, x, other_right), y,
                          Two(w_left, w, w_right))


  (* Downward phase for inserting (k,v) into our dictionary d.
   * The downward phase returns a "kicked" up configuration, where
   *
   * type kicked =
   *      | Up of dict * pair * dict
   *      | Done of dict
   *
   * A kicked up configuration can only be a Two node, hence the Up
   * constructor takes the same parameters as the Two constructor. We return
   * Up(left,(k,v),right) if the Two-node represented by this Up needs to
   * be further kicked up in the upward phase (this is represented by an up
   * arrow on the 2-3 Tree handout). We return Done(d) if we have finished
   * our upward phase on the tree represented by d.
   *
   * The functions insert_downward, insert_downward_two, and
   * insert_downward_three are __mutually recursive__, hence the
   * "let rec" and the "and" keywords. Here, we use three mutually recursive
   * functions to simplify our code into smaller pieces.
   *
   * Two functions f and g are __mutually recursive__ if in f's definition,
   * f calls g, and in g's definition, g calls f. This definition of
   * mutually recursive definitions can be extended to more than two functions,
   * as follows:
   *
   * Functions f1, f2, f3, ..., fn are mutually recursive if for each of
   * these functions f, all of the other f_i's can be called on some execution
   * of f. *)

  (* insert_downward should handle the base case when inserting into a Leaf,
   * and if our dictionary d is a Two-node or a Three-node, we call the
   * corresponding functions insert_downward_two or insert_downward_three
   * with the appropriate arguments. *)
  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up((Leaf,(k,v),Leaf))
      | Two(left,n,right) ->
        insert_downward_two (k,v) n left right
      | Three(left,n1,middle,n2,right) ->
        insert_downward_three (k,v) n1 n2 left middle right

  (* Downward phase on a Two node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) is the (key,value) of the current Two node, and left and right
   * are the two subtrees of the current Two node. *)
  and insert_downward_two ((k,v): pair) ((k1,v1): pair)
      (left: dict) (right: dict) : kicked =
      match D.compare k k1 with
      |Eq -> Done(Two(left, (k, v), right))
      |Less ->
        (match insert_downward left k v with
        | Up(l,n,r) -> insert_upward_two n l r (k1,v1) right
        | Done(dic) ->  Done(Two(dic,(k1,v1),right)))
      |Greater ->
        (match insert_downward right k v with
        | Up(l,n,r) ->
          insert_upward_two n l r (k1,v1) left
        | Done(dic) ->
        Done(Two(left, (k1,v1), dic)))

  (* Downward phase on a Three node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) and (k2,v2) are the two (key,value) pairs in our Three node, and
   * left, middle, and right are the three subtrees of our current Three node *)
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =
    match (D.compare k k1), (D.compare k k2) with
    | Eq, _ -> Done(Three(left, (k,v), middle, (k2,v2), right))
    | _, Eq -> Done(Three(left, (k1,v1), middle, (k,v), right))
    | Less, _ ->
      (match insert_downward left k v with
      | Up(l,n,r) ->
        insert_upward_three n l r (k1,v1) (k2,v2) middle right
      | Done(dic) -> Done(Three(dic,(k1,v1),middle,(k2,v2),right)) )
    | Greater, Less ->
      (match insert_downward middle k v with
      | Up(l,n,r)  ->
        insert_upward_three n l r (k1,v1) (k2,v2) left right
      | Done(dic)  -> Done(Three(left,(k1,v1),dic,(k2,v2),right)) )
    | _, Greater ->
      (match insert_downward right k v with
      | Up(l,n,m)  ->
      insert_upward_three n l m (k1,v1) (k2,v2) left middle
      | Done(dic)  ->
      Done(Three(left,(k1,v1),middle,(k2,v2),dic)) )


  (* We insert (k,v) into our dict using insert_downward, which gives us
   * "kicked" up configuration. We return the tree contained in the "kicked"
   * configuration. *)
  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

  (* Upward phase for removal where the parent of the hole is a Two node.
   * See cases (1-2) on the handout. n is the (key,value) pair contained in
   * the parent node; left and right are the subtrees of the parent node (our
   * hole is one of these subtrees); and dir indicates which subtree was
   * contained by the hole. *)
  let remove_upward_two (n: pair) (rem: pair option)
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  (* Upward phase for removal where the parent of the hole is a Three node.
   * See cases (3-4) on the handout. n1 and n2 are the (key,value) pairs
   * contained in the parent node; left, middle, and right are the subtrees
   * of the parent node (our hole is one of these subtrees); and dir indicates
   * which subtree was the tree contained by the hole. *)
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e -> Absorbed(rem,Three(Two(a,w,b),
                                                      x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e -> Absorbed(rem,Three(Two(a,w,b),
                                                      x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) -> Absorbed(rem,Three(a,w,Two(b,x,c),
                                                      y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e -> Absorbed(rem,Three(a,w,Two(b,x,c),
                                                      y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Eq -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Eq, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Eq -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) ->
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less ->
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  (* DO NOT EDIT THIS *)
  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Eq, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) ->
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) ->
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) ->
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) ->
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  (* DO NOT EDIT THIS *)
  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  (* TODO:
   * Write a lookup function that returns the value of the given key
   * in our dictionary and returns it as an option, or return None
   * if the key is not in our dictionary. *)
  let rec lookup (d: dict) (k: key) : value option =
    match d with
    | Leaf -> None
    | Two(Leaf,(k1,v1),Leaf) -> if (D.compare k k1)=Eq then Some v1 else None
    | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
      if (D.compare k k1)=Eq then
        Some v1
      else if (D.compare k k2)=Eq then
        Some v2
      else
        None
    | Two(l,(k1,v1),r) ->
      if (D.compare k k1)=Eq then
        Some v1
      else if (D.compare k k1)=Less then
        lookup l k
      else
        lookup r k
    | Three(l,(k1,v1),m,(k2,v2),r) ->
      if (D.compare k k1)=Eq then
        Some v1
      else if (D.compare k k2)=Eq then
        Some v2
      else if (D.compare k k1)=Less then
        lookup l k
      else if (D.compare k k2)=Less then
        lookup m k
      else
        lookup r k


  (* TODO:
   * Write a function to test if a given key is in our dictionary *)
  let member (d: dict) (k: key) : bool =
    match lookup d k with
    | None -> false
    | Some v -> true

  (* TODO:
   * Write a function that removes any (key,value) pair from our
   * dictionary (your choice on which one to remove), and returns
   * as an option this (key,value) pair along with the new dictionary.
   * If our dictionary is empty, this should return None. *)
  let choose (d: dict) : (key * value * dict) option =
    match d with
    | Leaf -> None
    | Two(l,(k,v),r) ->
      let new_d = remove d k in
      Some (k,v,new_d)
    | Three(l,(k,v),m,n2,r) ->
      let new_d = remove d k in
      Some (k,v,new_d)


  (* [tree_path_length d] returns an int of the length (which we consider
   * the total number of nodes from root to all of its leaves) of a tree
   * - [d] is the current root of the tree
   *
   * We know that a Leaf to itself is length 0, a Two to its leaves is length
   * 1. However, we consider a Three to it's leaves length 1 as well, and not
   * 2. This is so that we can account for when a tree has both Two AND Three
   * nodes, so we try to simulate Three nodes as Two nodes.
   *
   * This goes the same for the case when the root is a Three to other nodes,
   * We calculate the two path lengths of the left/middle and middle/right
   * trees, as if the left key and right key of that Three node were two
   * separate Two nodes. If those separate values are equal, then
   * transitively, we know that left/right would also be equal, and we
   * return the path_length for just one of those simulated Two nodes.
   * If they are not equal, then we return -1, which will always cause our
   * [balanced] function to fail, since we now know that left/middle/right
   * paths are not equal.
    *)
  let rec tree_path_length (d: dict) : int =
    match d with
      | Leaf -> 0
      | Two(Leaf,(k1,v1),Leaf) -> 1
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) -> 1
      | Two(l,n,r) -> 1+tree_path_length(l)+tree_path_length(r)
      | Three(l,n1,m,n2,r) ->
        if ((1+tree_path_length(l)+tree_path_length(m)=
            1+tree_path_length(m)+tree_path_length(r))) then
          1+tree_path_length(l)+tree_path_length(m)
        else
          -1

  (* TODO:
   * Write a function that when given a 2-3 tree (represented by our
   * dictionary d), returns true if and only if the tree is "balanced",
   * where balanced means that the given tree satisfies the 2-3 tree
   * length invariants stated above and in the 2-3 tree handout. *)

  (* How are you testing that you tree is balanced?
   * ANSWER:
   *   First, the base cases are a Leaf, a Two to Leaf, or a Three to leaf,
   *   Each of these base cases will be true, because a Leaf is length=0,
   *   a Two to a Leaf will have both directions have length=1,
   *   and a Three to Leaf will have all 3 directions have length=1.
   *
   *   For the case that the tree is a Two or Three to other types of nodes, we
   *   call a recursive helper function, tree_path_length, which computes
   *   the path lengths of the left, right, and possibly middle trees.
   *   Using this, for a Two, we only need to check that l and r have equal
   *   path lengths. For a Three, we must ensure that all three directions
   *   have equal path lengths. we can check just l=r and r=m, because
   *   transitively, if l=r and r=m, then l=m.
   *
   *   See the comments for tree_path_length for a description there.
   *)
  let balanced (d: dict) : bool =
    match d with
      | Leaf -> true
      | Two(Leaf,(k1,v1),Leaf) -> true
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) -> true
      | Two(l,n,r) -> tree_path_length(l)=tree_path_length(r)
      | Three(l,n1,m,n2,r) ->
        tree_path_length(l)=tree_path_length(r) &&
        tree_path_length(r)=tree_path_length(m)


  (********************************************************************)
  (*       TESTS                                                      *)
  (* You must write more comprehensive tests, using our remove tests  *)
  (* below as an example                                              *)
  (********************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  (* Tests fold on four cases, each using an incrementing function.
   * If the result of fold equals the number of pairs in the tree, fold is
   * successful *)
  let test_fold () =
    let d1 = Leaf in
    let fold1 = fold (fun k v a -> a+1) 0 d1 in
    assert(fold1=0) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    let fold2 = fold (fun k v a -> a+1) 0 d2 in
    assert(fold2 = 1) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    let fold3 = fold (fun k v a -> a+1) 0 d3 in
    assert(fold3 = 2) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    let fold4 = fold (fun k v a -> a+1) 0 d4 in
    assert(fold4 = 26) ;
    ()

    let test_string_of_key () =
    let k = D.gen_key() in
    assert((string_of_key k)=D.string_of_key k) ;
    ()

  let test_string_of_value () =
    let v = D.gen_value() in
    assert((string_of_value v)=D.string_of_value v) ;
    ()

  let test_string_of_dict () =
    let pair1 = D.gen_pair() in
    let d = Two(Leaf,pair1,Leaf) in
    let d_string = string_of_dict d in
    let correct_string = "Dict: Key: "^(string_of_key(fst pair1))^", Value: "^
          (string_of_value(snd pair1))^"\n" in
    assert(d_string = correct_string);
    ()

  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4) ;

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5)) ;

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6)) ;

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7)) ;
    ()

    let test_lookup () =
      let d1 = Leaf in
      let looked_up1 = lookup d1 (D.gen_key ()) in
      assert(looked_up1 = None);

      let k_d2 = D.gen_key() in
      let v_d2 = D.gen_value() in
      let d2 = insert empty k_d2 v_d2 in
      let looked_up2 = lookup d2 k_d2 in
      assert(looked_up2 = Some v_d2);

      let k1_d3 = D.gen_key () in
      let v1_d3 = D.gen_value () in
      let d3_old = insert empty k1_d3 v1_d3 in
      let d3 = insert d3_old (D.gen_key_random ()) (D.gen_value ()) in
      let looked_up3 = lookup d3 k1_d3 in
      assert(looked_up3 = Some v1_d3);
      let looked_up3_none = lookup d3 (D.gen_key_random()) in
      assert(looked_up3_none=None);

      let first_key = D.gen_key() in
      let second_key = D.gen_key_gt first_key () in
      let second_value = D.gen_value () in
      let third_key = D.gen_key_gt second_key () in
      let fourth_key = D.gen_key_gt third_key () in
      let fourth_value = D.gen_value () in
      let d4_old = insert empty second_key second_value in
      let d4_middle = insert d4_old fourth_key fourth_value in
      let d4_end = insert d4_middle (D.gen_key_random ()) (D.gen_value ()) in
      let lookedup4_second = lookup d4_end second_key in
      assert(lookedup4_second = Some second_value);
      let lookedup4_fourth = lookup d4_end fourth_key in
      assert(lookedup4_fourth = Some fourth_value);
      assert(lookup d4_end (D.gen_key_random()) = None);
      ()

  let test_member () =
    let d1 = Leaf in
    assert(not(member d1 (D.gen_key())));

    let k_d2 = D.gen_key() in
    let v_d2 = D.gen_value() in
    let d2 = insert empty k_d2 v_d2 in
    assert(member d2 k_d2 );

    let k1_d3 = D.gen_key() in
    let v1_d3 = D.gen_value() in
    let d3 = insert (insert empty k1_d3 v1_d3) (D.gen_key_random ())
                    (D.gen_value ()) in
    assert(member d3 k1_d3);
    assert(not(member d3 (D.gen_key_random())));

    let first_key = D.gen_key() in
    let second_key = D.gen_key_gt first_key () in
    let third_key = D.gen_key_gt second_key () in
    let fourth_key = D.gen_key_gt third_key () in
    let d4 = insert
                (insert
                      (insert
                            (insert empty fourth_key (D.gen_value()))
                      third_key (D.gen_value()))
                second_key (D.gen_value()))
             first_key (D.gen_value()) in
    assert(member d4 first_key);
    assert(member d4 third_key);
    assert(not(member d4 (D.gen_key_random())));
    ()

  let test_insert_upward_two_wx () =
    (* Check the kick up for w<X *)
    let x_key = D.gen_key() in
    let x_val = D.gen_value() in
    let x = (x_key,x_val) in
    let w_key = D.gen_key_lt x_key () in
    let w_val = D.gen_value() in
    let w = (w_key,w_val) in
    let x_other = Leaf in
    let w_left = Leaf in
    let w_right = Leaf in
    let three_wx = insert_upward_two w w_left w_right x x_other in
    match three_wx with
    | Done(Three(Leaf, left_pair, Leaf, right_pair, Leaf)) ->
      assert(D.compare (fst left_pair) w_key = Eq) ;
      assert(D.compare (fst right_pair) x_key = Eq) ;
      ()
    | _ -> failwith "We shouldn't be here"

  let test_insert_upward_two_xw () =
    (* Check the kick up for x<w *)
    let x_key = D.gen_key() in
    let x_val = D.gen_value() in
    let x = (x_key,x_val) in
    let w_key = D.gen_key_gt x_key () in
    let w_val = D.gen_value() in
    let w = (w_key,w_val) in
    let x_other = Leaf in
    let w_left = Leaf in
    let w_right = Leaf in
    let three_wx = insert_upward_two w w_left w_right x x_other in
    match three_wx with
    | Done(Three(Leaf, left_pair, Leaf, right_pair, Leaf)) ->
      assert(D.compare (fst left_pair) x_key = Eq) ;
      assert(D.compare (fst right_pair) w_key = Eq) ;
      ()
    | _ -> failwith "We shouldn't be here"

  (*helper function that matches nodes and leaves for inserting upward phase
   *compares the nodes and inserts it respectively*)
  let match_insert_upward_three_helper new_node root_key left_key
        right_key leaf1 leaf2 leaf3 leaf4 () =
    match new_node with
    | Up(left, (node_key, node_val), right) ->
      assert(D.compare node_key root_key = Eq);
      let test_left =
        (match left with
        | Two(a, (l_key, left_val), b) ->
          assert(D.compare l_key left_key = Eq);
          let test_a =
            (match a with
            | Two(Leaf, (a_key,a_val), Leaf) ->
              assert(D.compare a_key leaf1 = Eq);
              ()
            | _ -> failwith "We shouldn't be here")
          in
          let test_b =
            (match b with
            | Two(Leaf, (b_key, b_val), Leaf) ->
              assert(D.compare b_key leaf2 = Eq);
              ()
            | _ -> failwith "We shouldn't be here") in
            let () = test_a in
            test_b
        | _ -> failwith "We shouldn't be here")
      in

      let test_right =
      (match right with
      | Two(c, (r_key, right_val), d) ->
        assert(D.compare r_key right_key = Eq);
        let test_c =
          (match c with
          | Two(Leaf, (c_key,c_val), Leaf) ->
            assert(D.compare c_key leaf3 = Eq);
            ()
          | _ -> failwith "We shouldn't be here")
        in
        let test_d =
          (match d with
          | Two(Leaf, (d_key, d_val), Leaf) ->
            assert(D.compare d_key leaf4 = Eq);
            ()
          | _ -> failwith "We shouldn't be here")
        in
        let () = test_c in
        test_d
      | _ -> failwith "We shouldn't be here")
      in
      let () = test_left in
      test_right
    | _ -> failwith "We shouldn't be here"

  (* Check the kick up for first case, w<X<Y *)
  let test_insert_upward_three_wxy () =
    let w_key = D.gen_key() in
    let w = (w_key, D.gen_value()) in
    let x_key = D.gen_key_gt w_key () in
    let x = (x_key, D.gen_value()) in
    let y_key = D.gen_key_gt x_key () in
    let y = (y_key, D.gen_value()) in
    let w_left_key = D.gen_key_random() in
    let w_left = Two(Leaf,(w_left_key,D.gen_value()),Leaf) in

    let w_right_key = D.gen_key_random() in
    let w_right = Two(Leaf,(w_right_key,D.gen_value()),Leaf) in

    let other_left_key = D.gen_key_random() in
    let other_left = Two(Leaf,(other_left_key,D.gen_value()),Leaf) in

    let other_right_key = D.gen_key_random() in
    let other_right = Two(Leaf,(other_right_key,D.gen_value()),Leaf) in
    let new_node = insert_upward_three w w_left w_right
                                      x y other_left other_right in
    match_insert_upward_three_helper new_node x_key w_key y_key
                                    w_left_key w_right_key
                                    other_left_key other_right_key ()


(* Check the kick up for second case, x<w<Y *)
  let test_insert_upward_three_xwy () =
    let x_key = D.gen_key() in
    let x = (x_key, D.gen_value()) in
    let w_key = D.gen_key_gt x_key () in
    let w = (w_key, D.gen_value()) in
    let y_key = D.gen_key_gt w_key () in
    let y = (y_key, D.gen_value()) in
    let x_left_key = D.gen_key_random() in
    let x_left = Two(Leaf,(x_left_key,D.gen_value()),Leaf) in

    let x_right_key = D.gen_key_random() in
    let x_right = Two(Leaf,(x_right_key,D.gen_value()),Leaf) in

    let other_left_key = D.gen_key_random() in
    let other_left = Two(Leaf,(other_left_key,D.gen_value()),Leaf) in

    let other_right_key = D.gen_key_random() in
    let other_right = Two(Leaf,(other_right_key,D.gen_value()),Leaf) in
    let new_node = insert_upward_three x x_left x_right
                                      w y other_left other_right in
    match_insert_upward_three_helper new_node w_key x_key y_key
                                    x_left_key x_right_key
                                    other_left_key other_right_key ()


(* Check the kick up for third case, x<y<w *)
  let test_insert_upward_three_xyw () =
    let x_key = D.gen_key() in
    let x = (x_key, D.gen_value()) in
    let y_key = D.gen_key_gt x_key () in
    let y = (y_key, D.gen_value()) in
    let w_key = D.gen_key_gt y_key () in
    let w = (w_key, D.gen_value()) in
    let x_left_key = D.gen_key_random() in
    let x_left = Two(Leaf,(x_left_key,D.gen_value()),Leaf) in

    let x_right_key = D.gen_key_random() in
    let x_right = Two(Leaf,(x_right_key,D.gen_value()),Leaf) in

    let other_left_key = D.gen_key_random() in
    let other_left = Two(Leaf,(other_left_key,D.gen_value()),Leaf) in

    let other_right_key = D.gen_key_random() in
    let other_right = Two(Leaf,(other_right_key,D.gen_value()),Leaf) in

    let new_node = insert_upward_three x x_left x_right
                                      y w other_left other_right in
    match_insert_upward_three_helper new_node y_key x_key w_key
                                    x_left_key x_right_key
                                    other_left_key other_right_key ()

  (* Test basic insertions into empty and 1-level Two/Three nodes *)
  let test_insert_basics () =
    (* Insert element into empty tree *)
    let dic = empty in
    let k1 = D.gen_key() in
    let v1 = D.gen_value() in
    let one_elt_dic = insert dic k1 v1 in
    assert(one_elt_dic = Two(Leaf, (k1,v1), Leaf));
    (* Insert element k2>k1 into Two with node k1
    * should be Three with k1 then k2 in node *)
    let k4 = D.gen_key_gt k1 () in (* to be used later *)
    let k2 = D.gen_key_gt k4 () in
    let v2 = D.gen_value() in
    let two_elt_dic = insert one_elt_dic k2 v2 in
    assert(two_elt_dic = Three(Leaf,(k1,v1), Leaf, (k2,v2), Leaf));
    (* Insert element k2 with different value, should update value *)
    let v2_2 = D.gen_value () in
    let updated_two_elt_dic = insert two_elt_dic k2 v2_2 in
    assert(updated_two_elt_dic = Three(Leaf,(k1,v1), Leaf, (k2,v2_2), Leaf));
    (* Insert element k3, where k3<k1<k2 into Three with k1 and k2
    * should be Two with k1 as node, k3 left, k2 right *)
    let k3 = D.gen_key_lt k1 () in
    let v3 = D.gen_value() in
    let three_elt_dic_1 = insert two_elt_dic k3 v3 in
    assert(three_elt_dic_1 = Two(Two(Leaf,(k3,v3),Leaf),(k1,v1),
                                Two(Leaf,(k2,v2),Leaf)));
    (* Insert element k4, initialized earlier, where k1<k4<k2 into
    * Three with k1 and k2. Should be Two with k4 as node, k1 left, k2 right *)
    let v4 = D.gen_value() in
    let three_elt_dic_2 = insert two_elt_dic k4 v4 in
    assert(three_elt_dic_2 = Two(Two(Leaf,(k1,v1),Leaf),(k4,v4),
                                Two(Leaf,(k2,v2),Leaf)));
    (* Insert element k5, where k1<k2<k5 into Three with k1 and k2
    * Should return Two with k2 as node, k1 left, k5 right *)
    let k5 = D.gen_key_gt k2 () in
    let v5 = D.gen_value () in
    let three_elt_dic_3 = insert two_elt_dic k5 v5 in
    assert(three_elt_dic_3 = Two(Two(Leaf,(k1,v1),Leaf),(k2,v2),
                                Two(Leaf,(k5,v5),Leaf)));
    (* Insert another element k6, k6>k5
    * Should return Two with k2 as node, k1 left Two, k5 k6 right Three *)
    let k6 = D.gen_key_gt k5 () in
    let v6 = D.gen_value () in
    let four_elt_dic = insert three_elt_dic_3 k6 v6 in
    assert(four_elt_dic = Two(Two(Leaf,(k1,v1),Leaf),(k2,v2),
                              Three(Leaf,(k5,v5),Leaf,(k6,v6),Leaf)));

    (* Tests insertion of duplicate into three node *)
    let v6_copy = D.gen_value () in
    let four_elt_dic_copy = insert four_elt_dic k6 v6_copy in
    assert(four_elt_dic_copy = Two(Two(Leaf,(k1,v1),Leaf),(k2,v2),
                          Three(Leaf,(k5,v5),Leaf,(k6,v6_copy),Leaf)));

    (* Tests inserting another element, k7, k7>k6
    * Should return Two with 2 Twos which also point to 2 Twos each *)
    let k7 = D.gen_key_gt k6 () in
    let v7 = D.gen_value () in
    let five_elt_dic = insert four_elt_dic k7 v7 in
    assert(five_elt_dic = Three(Two(Leaf,(k1,v1),Leaf),(k2,v2),
                                Two(Leaf,(k5,v5),Leaf),(k6,v6),
                                Two(Leaf,(k7,v7),Leaf)));

    (* Test insertion of duplicate key k6 to five_elt_dic to update value
    * ensures that we can update value in middle of tree *)
    let five_elt_dic_copy = insert five_elt_dic k6 v6_copy in
    assert(five_elt_dic_copy = Three(Two(Leaf,(k1,v1),Leaf),(k2,v2),
                              Two(Leaf,(k5,v5),Leaf),(k6,v6_copy),
                              Two(Leaf,(k7,v7),Leaf)));
    ()

  (* Test insertion into larger trees *)
  let test_insert () =
    (* Test the insertion will always be balanced *)
    let pairs1 = generate_pair_list 5 in
    let d1 = insert_list empty pairs1 in
    assert(balanced d1);
    let pairs2 = generate_pair_list 6 in
    let d2 = insert_list empty pairs2 in
    assert(balanced d2);
    let pairs3 = generate_pair_list 7 in
    let d3 = insert_list empty pairs3 in
    assert(balanced d3);
    let pairs3 = generate_random_list 197 in
    let d3 = insert_list empty pairs3 in
    assert(balanced d3);
    ()

  let test_remove_nothing () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    let r2 = remove d1 (D.gen_key_lt (D.gen_key()) ()) in
    List.iter (fun (k,v) -> assert(lookup r2 k = Some v)) pairs1 ;
    assert(balanced r2) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter
      (fun (k,v) ->
        let r = remove d1 k in
        let _ = List.iter
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    List.iter
      (fun (k,v) ->
        let r = remove d1 k in
        let _ = List.iter
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    let r5 = List.fold_right (fun (k,_) d -> remove d k) pairs5 d5 in
    List.iter (fun (k,_) -> assert(not (member r5 k))) pairs5 ;
    assert(r5 = empty) ;
    assert(balanced r5) ;
    ()

  let test_choose () =
    (* Test on choose from empty *)
    let d1 = empty in
    let choose1 = choose d1 in
    assert(choose1=None);

    (* Test on choose from one elt -- Two to 2 Leafs *)
    let k2 = D.gen_key () in
    let v2 = D.gen_value () in
    let d2 = insert empty k2 v2 in
    let choose2 = choose d2 in
    assert(choose2 = Some(k2,v2,empty));

    (* Test on choose from two elt -- Three to 3 Leafs *)
    let k3 = D.gen_key () in
    let v3 = D.gen_value () in
    let old_d3 = insert empty (D.gen_key_gt k3 ()) (D.gen_value ()) in
    let d3 = insert old_d3 k3 v3 in
    let choose3 = choose d3 in
    assert(choose3 = Some(k3,v3,old_d3));

    (* Test on choose from three elt -- Two to 2 Twos to Leaves
    * Should return a Three node *)
    let k4 = D.gen_key () in
    let v4 = D.gen_value () in
    let k4_below = D.gen_key_lt k4 () in
    let v4_below = D.gen_value () in
    let k4_above = D.gen_key_gt k4 () in
    let v4_above = D.gen_value () in
    let old_d4 = insert (insert empty k4_below v4_below) k4_above v4_above in
    let d4 = insert old_d4 k4 v4 in
    let choose4 = choose d4 in
    assert(choose4 = Some(k4,v4,old_d4));

    (* Test on choose from four elt -- Three to 3 Twos to Leaves
    * Should return a Two to a Two and a Three to Leaves *)
    let k1 = D.gen_key_lt k2 () in
    let v1 = D.gen_value () in
    let k5 = D.gen_key () in
    let v5 = D.gen_value () in
    let old_d5 = Two(Three(Leaf, (k1,v1), Leaf,(k3,v3),Leaf),
                                 (k4,v4),Two(Leaf,(k5,v5),Leaf)) in
    let new_d5 = Three(Two(Leaf,(k1,v1),Leaf),(k2,v2),Two(Leaf,(k3,v3),Leaf),
                                (k4,v4),Two(Leaf,(k5,v5),Leaf)) in
    let choose5 = choose new_d5 in
    assert(choose5 = Some(k2,v2,old_d5));
    ()

  let run_tests () =
   test_balance() ;
   test_fold() ;
   test_string_of_key() ;
   test_string_of_value () ;
   test_string_of_dict () ;
   test_lookup () ;
   test_member () ;
   test_insert_upward_two_wx () ;
   test_insert_upward_two_xw () ;
   test_insert_upward_three_wxy () ;
   test_insert_upward_three_xwy () ;
   test_insert_upward_three_xyw () ;
   test_insert_basics () ;
   test_insert () ;
   test_remove_nothing() ;
   test_remove_from_nothing() ;
   test_remove_in_order() ;
   test_remove_reverse_order() ;
   test_remove_random_order() ;
   test_choose () ;
   ()

end

(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our
 * AssocListDict functor and run the tests *)
module IntStringListDict = AssocListDict(IntStringDictArg)
let _ = IntStringListDict.run_tests()

(* Create a dictionary mapping ints to strings using our
 * BTDict functor and run the tests.
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 tree implementation. *)

module IntStringBTDict = BTDict(IntStringDictArg)
let _ = IntStringBTDict.run_tests()


(******************************************************************)
(* Make: a functor that creates a DICT by calling our             *)
(* AssocListDict or BTDict functors                               *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
  (* Change this line to the BTDict implementation when you are
   * done implementing your 2-3 trees. *)
  (* AssocListDict(D) *)
  BTDict(D)

