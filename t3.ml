let rec sum l =
        match l with
          | []->0
          | x::xs->x + sum xs
;;

let rec append l1 l2 =
       match l1 with
         | []->l2
         | x::xs -> x::append xs l2
;;

let rec putAtEnd l v =
       match l with 
         | []->[v] 
         | x::xs -> x:: putAtEnd xs v
;;

let rec reverse l = 
        match l with
          | []->[]
          | x::xs -> putAtEnd (reverse xs) x;;
;;

let rec maxList l =
        match l with
          | [] -> failwith "Lista vazia"
          | [x] -> x
          | x::xs -> max x (maxList xs)
;;

let rec map l f = 
        match l with
          | []->[]
          | x::xs -> f x::map xs f
;;

let rec filter l f = 
        match l with
          | [] -> []
          | x::xs -> if f x 
                     then x::filter xs f 
                     else filter xs f
;;

let rec flatMap l f =
        match l with
          | [] -> []
          | x::xs -> f x @ flatMap xs f
;;

let rec insert ol v = (* ol is an ordered list*)
        match ol with
          | [] -> [v]
          | x::xs -> if x > v 
                     then v::ol
                     else x::insert xs v
;;

let rec sort l =
        match l with
          | [] -> []
          | x::xs -> insert (sort xs) x
;;

let rec fusion l1 l2 =
        match l1, l2 with
          | [],_ -> l2
          | _,[] -> l1
          | x::xs,y::ys -> if y>x  then x:: fusion xs l2 else y::fusion l1 ys
;; 
