(* Cat 1 *)
let rec halfHalf l =
        match l with 
          | [] -> ([],[])
          | x::xs -> let (us,vs) = halfHalf xs in
                         (x::vs,us)

(* Cat 2 *)                         
let rec halfHalf2 l =
        match l with
          | [] -> ([],[])
          | [x] -> ([x],[])
          | x::y::xs -> let (us,vs) = halfHalf2 xs in (x::us,y::vs)
                 
(* Cat 3 *)          
let rec partition a l =
        match l with
          | [] -> ([],[])
          | x::xs -> let (us,uv) = partition a xs in
                if x>a then (us,x::uv) else (x::us,uv)

let rec quickSort l =
       match l with 
         | [] -> []
         | x::xs -> let (us,vs) = partition x xs in
                quickSort us @ [x] @ quickSort vs

let rec removeFirstFromList a l =
        match l with 
          | [] -> [] 
          | x::xs -> if x=a then xs else x::removeFirstFromList a xs

let rec minList l =
        match l with
          | [] -> failwith "Erro, lista vazia"
          | [x] -> x
          | x::xs -> min x (minList xs)

let rec minSort l =
        match l with
          | [] -> []
          | lst -> let y = minList lst in
               y :: minSort (removeFirstFromList y lst)


let rec hasDiv n a z =
        if a > z then false
        else (n mod a = 0) || hasDiv n (n + 1) z

let rec isPrime n =
        n>1 && not (hasDiv n 2 (n - 1))

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec maxList l =
        match l with 
          | [] -> failwith "Erro: Lista vazia!"
          | [x] -> x
          | x::xs -> max x (maxList xs)

let rec sumList l1 l2 =
        match l1,l2 with
          | [],[] -> []
          | [],l -> l
          | l, [] -> l
          | x::xs, y::ys -> x + y :: sumList xs ys
       
let rec levels t = 
        match t with
          | Nil -> []
          | Node(_,l,r) -> 1::sumList (levels l) (levels r)

let rec width t = if t = Nil then 0 else maxList (levels t) 
