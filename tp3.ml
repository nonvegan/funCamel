type 'a tree = Nil | Node of 'a * 'a tree * 'a tree ;;

let rec howMany v t = 
        match t with
          | Nil -> 0
          | Node(x,l,r) -> (if v=x then 1 else 0) + howMany v l + howMany v r
;;

let rec eqPairs t =
        match t with
          | Nil -> 0
          | Node((x,y),l,r)  -> (if x=y then 1 else 0) + eqPairs l + eqPairs r
;;

let rec treeToList t =
        match t with
          | Nil -> []
          | Node(x,l,r) -> [x] @ treeToList l @ treeToList r
;;

let rec height t =
        match t with
          | Nil -> 0
          | Node(_,l,r) -> 1 + max (height l) (height r)
;;

let rec isBalanced t =
        match t with
          | Nil-> true
          | Node(_,l,r) -> abs(height l - height r) < 2 && isBalanced l && isBalanced r  
;;

let rec subtrees t =
        match t with
          | Nil -> [Nil]
          | Node(_,l,r) -> t:: subtrees l @ subtrees r           
;;

let rec fall t =
        match t with
          | Nil -> Nil
          | Node(_,Nil,Nil) -> Nil
          | Node(x,l,r) -> Node(x, fall l, fall r)  
;;

let rec spring v t = 
        match t with
          | Nil -> Node(v,Nil,Nil)
          | Node(x,l,r) -> Node(x, spring v l, spring v r)
;; 

type 'a ntree = NNil | NNode of 'a * 'a ntree list ;;

let rec nTreeToList nt =
        match nt with 
          | NNil -> []
          | NNode (x,tl) -> x:: lnTreeToList tl 
and lnTreeToList tl = 
        match tl with
          | [] -> []
          | x::xs -> nTreeToList x @ lnTreeToList xs
;;

let rec nFall nt = 
        match nt with
          | NNil -> NNil
          | NNode(_,[]) -> NNil
          | NNode(x,ntl) -> NNode(x, lnFall ntl) 
and lnFall ln = 
        match ln with 
          | [] -> []
          | x::xs -> nFall x :: lnFall xs
;;

let ncons t l = if t = NNil then l else t::l ;;


let rec nSpring v nt =
        match nt with
          | NNil -> NNode(v,[])
          | NNode(x,[]) -> NNode(x,[NNode(v,[])])
          | NNode(x,lnt) -> NNode (x, lnSpring v lnt)
and lnSpring v lnt =
        match lnt with
          | [] -> []
          | x::xs -> ncons (nSpring v x)  (lnSpring v xs)
;;


