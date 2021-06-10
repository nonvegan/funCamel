let rec succAll l = 
        match l with
          |[]->[]
          |x::xs -> x+1::succAll xs
;;

let rec belongs l a =
        match l with
          | []->false
          | x::xs -> x=a || belongs xs a
;;

let rec union lx ly = 
        match lx with
          | []->ly
          | x::xs -> if belongs ly x then union xs ly else x::union xs ly

;;

let rec inter lx ly =
        match lx with
          | []->[]
          | x::xs -> if belongs ly x then x::inter xs ly  else inter xs ly
;;

let rec diff lx ly = 
        match lx with 
          | []->[]
          | x::xs -> if belongs ly x then diff xs ly else x::diff xs ly 
;;

let rec insert l v =
        match l with
          | []->[]
          | x::xs -> (v::x)::insert xs v 
;;

let rec power l = 
        match l with
          | [] -> [[]]
          | x::xs -> insert (power xs) x @ power xs
;;


let rec nat n = 
        if n=0 then [] else n - 1:: nat (n - 1)
;;
