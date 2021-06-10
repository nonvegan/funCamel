
let rec print_list l = 
        match l with
              | [] -> ()
              | x::xs -> print_int x; print_newline ();print_list xs
;;
             
