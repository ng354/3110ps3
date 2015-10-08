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
let modify_link_set (l : link) (d : WordDict.dict) (wrd : string)
                     : WordDict.dict =
  let new_set =
  match WordDict.lookup d wrd with
  |None -> LinkSet.singleton l
  |Some set -> (LinkSet.insert l set) in
    WordDict.insert d wrd new_set

(*adding the keys and the modified linksets in the dictionary*)
let rec init_dic dict ky_lst lnk : WordDict.dict =
match ky_lst  with
|[] -> dict
|h::t -> init_dic (modify_link_set lnk dict h) t lnk

(*Returns option*)
let get_opt opt =
  match opt with
  | None -> raise (Invalid_argument "Option.get")
  | Some x -> x

(*adds link to the frontier if it doesn't exist already*)
let rec add_link_to_frontier (lst : link list) (frnt : LinkSet.set)
                              (visited : LinkSet.set) =
  match lst with
  | [] -> frnt
  | h::t ->
    if member visited h then
      add_link_to_frontier t frnt visited
    else
      add_link_to_frontier t (insert h frnt) visited

(*removes link from frontier after it is visited*)
let remove_link_from_front (lnk : link) (frnt : LinkSet.set)
                          (visited : LinkSet.set) =
  (*link list of all links on that page*)
  let page_link = get_opt(get_page lnk) in
  let updated_frontier = add_link_to_frontier page_link.links frnt visited in
  remove lnk updated_frontier

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
 don't go to it and continue. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  if n=0 || is_empty(frontier) then d
  else
    let (chosen, new_front) = get_opt(choose frontier) in
    (*the page for that link*)
    match get_page chosen with
    | Some page ->
         let chosens_page = page in
        if member visited chosen then
          crawl n new_front visited d
        else
        let page_word_list = chosens_page.words in
        let updated_dict  = init_dic d page_word_list chosen in
        let updated_front = remove_link_from_front chosen frontier visited in
        let updated_visit = insert chosen visited in
          crawl (n-1) updated_front updated_visit updated_dict
    | None ->
      crawl n new_front visited d

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty


(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
