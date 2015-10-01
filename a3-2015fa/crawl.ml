open Util
open CrawlerServices
open Order
open Pagerank
open LinkSet



(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all()


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)


(*if word does not exist, create a new linkset with the link, otherwise
 * add the link to the current linkset for the word key *)
let modify_link_set (l : link) (d : WordDict.dict) (wrd : string) : WordDict.dict =
  let new_set =
  match WordDict.lookup d wrd with
  |None -> LinkSet.singleton l
  |Some set -> (LinkSet.insert l set) in
  WordDict.insert d wrd new_set

(*adding the keys and the modified linksets in the dictionary*)
let rec init_dic dict ky_lst lnk : dict =
match ky_lst  with
|[] -> dict
|h::t -> init_dic (modify_link_set lnk dict h) t lnk

(*removes duplicate words from the page words list*)
let rec remove_duplicates lst =
match lst with
|[] -> []
|h::t -> h::(remove_duplicates (List.filter (fun x -> x <> h) t))

let remove_link_from_front (lnk: link) (frnt: LinkSet.set) =
    (*link list of all links on that page*)
    let updated_frontier = add_link_to_frontier (get_page lnk).links frnt in
    remove lnk updated_frontier

(*adds link to the frontier if it doesnt exist already*)
let rec add_link_to_frontier (lst : link list) (frnt : LinkSet.set) =
match lst with
|[] -> frnt
|h::t -> add_link_to_frontier t (insert h frnt)


(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty.
 * match on frontier and check if it exists in LinkSet. If it exists then you
 don't go to it and continue.
*)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =

  (*add chosen to visited and check chosen's other page links and see if those
  have to be added in frontier

  use compare when adding the new key into the dictionary

  when n reaches 0 and when frontier is empty just return d the dictionary   *)

  (*chosen is a link*)
  let (chosen, new_front) = choose frontier in
  (*the page for that link*)
  let chosens_page = get_page chosen in
  (*list of string of all words on that page*)
  let page_word_list =  remove_duplicates chosens_page.words in
  (*updated dictionary with keys and link set values*)
  let updated_dict = init_dic d page_word_list chosen in

  let updated_front = remove_link_from_front chosen frontier in

  let updated_visit = insert chosen visited in


   crawl (n-1) updated_front updated_visit updated_dict

  (*stop when you reach n or when the frontier is empty*)



let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty


(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
