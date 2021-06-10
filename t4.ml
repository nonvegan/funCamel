type color = int ;;
type point = float*float ;;

type color_shape = 
        | Line of color*point*point
        | Circle of color*point*float
        | Rect of color*point*point
;;

let area cs =
        match cs with
          | Line(_,_,_) -> 0.0
          | Circle (_,_,r) -> 3.14 *. r *. r
          | Rect (_,(x0,y0),(x1,y1)) -> abs_float ((x1 -. x0) *. (y1 -. y0))
;;

let radius circle =
        match circle with 
          | Circle(_,_,r) -> r
;; 

let radius2 (Circle(_,_,r)) = r;;

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree ;;

let makeLeaf value =
        Node (value,Nil,Nil)
;;

let rec size tree =
        match tree with 
          | Nil -> 0
          | Node(_,l,r) -> 1 + size l + size r
;; 

let rec sum tree = 
        match tree with
          | Nil -> 0 
          | Node (x,l,r) -> x + sum l + sum r
;;

let rec depth tree = 
        match tree with
          | Nil -> 0
          | Node (_,l,r) -> 1 + max (depth l) (depth r)
;;

let rec mirror tree =
        match tree with
          | Nil -> Nil
          | Node(x,l,r)-> Node(x, mirror r, mirror l)
;;

type 'a ntree =  NNil | NNode of 'a * 'a ntree list ;; 

let makeNLeaf value = 
        NNode (value, [])
;;

let rec nsize ntree =
       match ntree with
         | NNil -> 0
         | NNode(_,tl) -> 1 + tlsize tl 
and tlsize tl = 
       match tl with
         | [] -> 0
         | x::xs -> nsize x + tlsize xs
;; 

let rec nsum ntree =
        match  ntree with
          | NNil -> 0 
          | NNode(x,lt) -> x + lnsum lt 
 and lnsum lt = 
        match lt with 
          | [] -> 0
          | x::xs -> nsum x + lnsum xs 
;;

let rec nmirror ntree =
        match ntree with
          | NNil -> NNil
          | NNode(x,lt) -> NNode(x, lnmirror lt)
and lnmirror lt =
        match lt with
          | [] -> []
          | x::xs -> lnmirror xs @ [x]
;;

