(* Genealogy module body *)

(* 
Aluno 1: ????? mandatory to fill
Aluno 2: ????? mandatory to fill

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
   100 columns
*)


(* COMPILATION - How to build this module (used by Mooshak))
         ocamlc -c Genealogy.mli Genealogy.ml
*)


(* AUXILIARY BASIC FUNCTIONS - you can add more *)

let rec uniq l =
	match l with
	|  [] -> []
	| [x] -> [x]
	| x::y::xs ->
		if x = y then uniq (y::xs)
		else x::uniq (y::xs)

let clean l = (* removes repetitions *)
	uniq (List.sort compare l)

let len =
	List.length

let map =
	List.map

let filter =
	List.filter

let mem =
	List.mem

let flatMap f l =
	List.flatten (map f l)

let partition =
	List.partition

let exists =
	List.exists

let for_all =
	List.for_all

let cFlatMap f l =
	clean (flatMap f l)

let union l1 l2 =
	clean (l1 @ l2)

let inter l1 l2 =
	filter (fun x -> mem x l2) l1

let diff l1 l2 =
	filter (fun a -> not (mem a l2)) l1



(* TYPES *)

type item = string * string list
type repository = item list

type aTree = ANil | ANode of string * aTree * aTree
type dTree = DNil | DNode of string * dTree list


(* EXAMPLES - you can add more *)

let example = [
           ("a", ["f";"g"]);
           ("b", ["f";"h"]);
           ("c", ["h";"i"]);
           ("f", ["g"; "j"]);
           ("g", ["j"]);
           ("h", []);
           ("i", []);
           ("j", [])
          ]


(* BASIC REPOSITORY FUNCTIONS - you can add more *)

let size rep = (* number of individuals *)
	len rep

let all1 rep = (* all the individuals *)
	map fst rep

let all2 rep = (* all the children (of anyone) *)
	cFlatMap snd rep

let roots rep = (* individuals without any parents *)
	diff (all1 rep) (all2 rep)

let inners rep = (* individuals with children *)
	let xs = filter (fun (p,cs) -> cs <> []) rep in
		all1 xs

let leaves rep = (* individuals without any children *)
	let xs = filter (fun (p,cs) -> cs = []) rep in
		all1 xs

let cut1 rep l = (* partition based on first component of the repository *)
	partition (fun (p,cs) -> mem p l) rep

let cut2 rep l = (* partition based on second component of the repository *)
	partition (fun (p,cs) -> inter cs l <> []) rep

let cut rep = (* partition -> (root pairs, rest pairs) *)
	cut1 rep (roots rep)

let children rep l = (* get all the children of the list l *)
	let (a,b) = cut1 rep l in
		all2 a

let rec parents rep l = (* get all the parents of the list l *)
	let (a,b) = cut2 rep l in
		all1 a

let rootAName aTree = match aTree with ANode(aName,_,_) -> aName

(* FUNCTION height *)

let rec height rep =
	match rep with
          | [] -> 0
          | lst -> let (roots,rest) = cut rep in 1 + height rest

(* FUNCTION makeATree *)

let rec makeATree rep a =
        let parentsA = parents rep [a] in
                match parentsA with 
                  | x::y::xs -> ANode(a,makeATree rep x, makeATree rep y)
                  | list -> ANode(a,ANil,ANil)

(* FUNCTION repOfATree ->wrong order, needs join func *)

let rec repOfATree t =
	match t with 
          | ANode(x,ANil,ANil) ->  [(x,[])]
          | ANode(x,a,ANil)-> [(x,[]);(rootAName a,[x])] @ repOfATree a
          | ANode(x,ANil,b) -> [(x,[]);(rootAName b,[x])] @ repOfATree b 
          | ANode(x,a,b)->[(x,[]);(rootAName a,[x]);(rootAName b,[x])]@ repOfATree a @ repOfATree b  


(* FUNCTION makeDTree *)

let rec makeDTree rep a =
        let childrenA = children rep [a] in
                match childrenA with
                  | [] -> DNode(a,[])
                  | list -> DNode(a,map (fun child -> makeDTree rep child) childrenA)  



(* FUNCTION repOfDTree *)

let repOfDTree t =
	[]


(* FUNCTION descendantsN *)

let rec descendantsN rep n lst =
        if n = 0
        then lst
        else descendantsN rep (n - 1) (children rep lst)     

(* FUNCTION siblings *)

let siblings rep lst = clean(lst @ children rep (parents rep lst))

let areSiblings rep a b = mem b (siblings rep [a]) 

(* FUNCTION siblingsInbreeding *)

let siblingsInbreeding rep =
        map (fun lstInbred -> match lstInbred with a::b::xs -> (a,b))
            (filter (fun lst -> match lst with a::b::xs -> areSiblings rep a b | _ -> false) 
                    (map (fun child -> clean(parents rep [child]))
                         (all2 rep)))


(* FUNCTION waveN *)

let waveN rep n lst =
	[]


(* FUNCTION merge *)

let merge rep1 rep2 =
	[]


(* FUNCTION supremum *)

let supremum rep s =
	[]


(* FUNCTION validStructural *)

let validStructural rep =
	false


(* FUNCTION validSemantic *)

let validSemantic rep =
	false

