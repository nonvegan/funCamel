#use "Genealogy.ml";;

print_newline ();;

let test name f res =
        if f = res
        then print_string ("Teste " ^ name ^ ": Pass!")
        else print_string ("Teste " ^ name ^ ": Fail!"); print_newline();;

let test_l name f res =
        if diff f res = diff res f 
        then print_string ("Teste " ^ name ^ ": Pass!")
        else print_string ("Teste " ^ name ^ ": Fail!"); print_newline();;



test "height" (height example) 4;;

test "makeATree" (makeATree example "g") (ANode ("g", ANode ("a", ANil, ANil),
 ANode ("f", ANode ("a", ANil, ANil), ANode ("b", ANil, ANil))));;

test_l "repOfATree" (repOfATree (ANode("g", ANode("a", ANil, ANil), ANode("f", ANode("a", ANil, ANil), ANode("b", ANil, ANil))))) [("a", ["f"; "g"]); ("b", ["f"]); ("f", ["g"]); ("g", [])];;

test "makeDTree" (makeDTree example "a") (DNode ("a",
 [DNode ("f", [DNode ("g", [DNode ("j", [])]); DNode ("j", [])]);
  DNode ("g", [DNode ("j", [])])]));;

test_l "repOfDTree" (repOfDTree (DNode("a", [DNode("f", [DNode("g", [DNode("j", [])]); DNode("j", [])]);  DNode("g", [DNode("j", [])])]))) [("a", ["f"; "g"]); ("f", ["g"; "j"]); ("g", ["j"]); ("j", [])];; 

test_l "descendantsN" (descendantsN example 1 ["a"]) ["f"; "g"];;

test_l "siblings" (siblings example ["g"]) ["f"; "g"; "j"];;

test_l "siblingsInbreeding" (siblingsInbreeding example) [("f", "g")];;

test_l "waveN" (waveN example 1 ["a"]) ["f";"g"];;

test_l "merge" (merge [("b", ["k"]); ("k", [])] example) [("b", ["f"; "h"; "k"]); ("k", []); ("a", ["f"; "g"]); ("c", ["h"; "i"]);
 ("f", ["g"; "j"]); ("g", ["j"]); ("h", []); ("i", []); ("j", [])];;

test_l "supremum" (supremum example ["h";"j"]) ["b"];;

test "validStructural" (validStructural example) true;;

test "validSemantic" (validSemantic example) true;;
