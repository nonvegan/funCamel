type tree = Nil | Node of int * tree * tree

let isEmpty t = t=Nil

let rec make l =
	match l with
  	| [] -> Nil
		| x::xs -> Node(x,Nil,make xs)

let max_int a b = if a > b then a else b

let rec max t = 
	match t with
	  | Nil -> failwith "max: empty tree"
	  | Node(x,Nil,Nil) -> x
		| Node(x,l,Nil) -> max_int x (max l)
		| Node(x,Nil,r) -> max_int x (max r)
	  | Node(x,l,r) -> max_int x (max_int (max l) (max r))

let rec loadChannel ci  =
	try 
			match input_line ci with
				| "-" -> Nil
				|  s  -> let  l = loadChannel ci in
								 	let r = loadChannel ci in
										Node(int_of_string s,l,r)
	with End_of_file -> failwith "loadChannel: premature end of file"

let load ni = 
	let ci = open_in ni in
		let t = loadChannel ci in
			close_in ci; t
			
let rec storeChannel co t =
	match t with
	  | Nil -> output_string co "-\n"
		| Node(x,l,r) -> output_string co (string_of_int x ^"\n");
										 storeChannel co l; storeChannel co r 
let rec store no t =
  	let co = open_out no in
  		storeChannel co t; close_out co; print_string ("Tree stored@"^no)
						
let rec showX t ident= 
	print_string ident;
	match t with
	  | Nil -> print_char '-'; print_newline()
		| Node(x,l,r) -> print_int x; print_newline();showX l (ident^" ");showX r (ident^" ")

let show t = showX t ""