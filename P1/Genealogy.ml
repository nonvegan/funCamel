(* Genealogy module body *)

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

let rec print_list l = match l with [] -> () | x::xs -> print_string x; print_list xs;;


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

let rec hasSomeone rep x = match rep with
                             | [] -> false
                             | (y,_)::yx -> y=x || hasSomeone yx x

let rootAName aTree = match aTree with ANode(aName,_,_) -> aName

let rootDName dTree = match dTree with DNode(dName,_) -> dName

let rec pairFromName rep name =
        match rep with
          | [] -> (name,[])
          | (key,values)::xs -> if name = key then (key,values) else pairFromName xs name 

let rec repFromNamesList rep l = 
        match l with 
          | [] -> []
          | x::xs -> pairFromName rep x::repFromNamesList rep xs

let rec lowestLevel rep = match cut rep with (root,[]) -> root | (_,rest) -> lowestLevel rest

let rec ancestors rep s =
        match s with
          | [] -> []
          | list -> clean(parents rep  s @ ancestors rep (parents rep s))

let commonAncestors rep s =
      match s with
        | [] -> []
        | list -> List.fold_left (inter) (all1 rep) (map (fun x -> ancestors rep [x]) s)


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

(* FUNCTION makeDTree *)

let rec makeDTree rep a =
        let childrenA = children rep [a] in
                match childrenA with
                  | [] -> DNode(a,[])
                  | list -> DNode(a,map (fun child -> makeDTree rep child) childrenA)  

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

let rec fullWaveN rep n lst = if n = 0 then lst else clean (lst @ fullWaveN rep (n - 1) (union (parents rep lst) (children rep lst)))

let waveN rep n lst = if n = 0 then lst else diff (fullWaveN rep n lst) (fullWaveN rep (n - 1) lst)

(* FUNCTION merge *)

let rec merge rep1 rep2 =
	match rep1 with
          | [] -> rep2
          | (key1,values1)::xs -> merge xs 
                                        (if hasSomeone rep2 key1 
                                        then map (fun (key2,values2)-> if key1 = key2 
                                                                       then (key2,union values1 values2)
                                                                       else (key2,values2))
                                                 rep2 
                                        else (key1,values1)::rep2)

(* FUNCTION supremum *)

let supremum rep s = all1(lowestLevel(repFromNamesList rep (commonAncestors rep s)))

(* FUNCTION validStructural *)

let rec validStructuralX rep originalRep =
        match rep with 
          | [] -> true
          | (root,children)::xs -> (not (hasSomeone xs root) && 
                                   List.fold_left (&&) true (map (fun y -> hasSomeone originalRep y) children)) && 
                                   validStructuralX xs originalRep

let validStructural rep = validStructuralX rep rep

(* FUNCTION validSemantic *)

let rec validSemantic rep =
	match rep with 
          | [] -> true
          | (root,children)::xs -> not(mem root children) && (len children <= 2) && validSemantic xs;;


(* FUNCTION repOfATree ->wrong order, needs join func *)

let rec repOfATree t = 
        match t with
          | ANode(x,ANil,ANil) -> [(x,[])]
          | ANode(x,ANode(y,l,r),ANil) ->  merge [(x,[]);(y,[x])] (repOfATree (ANode(y,l,r)))
          | ANode(x,ANil,ANode(y,l,r)) ->  merge [(x,[]);(y,[x])] (repOfATree (ANode(y,l,r)))
          | ANode(x,ANode(y,ly,ry),ANode(z,lz,rz)) -> merge [(x,[]);(y,[x]);(z,[x])] (merge (repOfATree (ANode(y,ly,ry))) (repOfATree (ANode(z,lz,rz))))

(* FUNCTION repOfDTree *)

let rec repOfDTree t =
	match t with 
          | DNode(x,[]) -> [(x,[])]
          | DNode(x,children) -> merge [(x,map rootDName children)] (List.fold_left (merge) [] (map repOfDTree children))

