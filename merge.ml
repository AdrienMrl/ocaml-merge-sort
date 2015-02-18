#! /usr/bin/ocaml

let input = 
    let input = [3; -10; -34; 999; 1; 9; 7; 23; 12; 11000; -21470000000; 48] in

let rec merge_sort f =

    (* there is a better way *)
    let halve l =
        let midlen = (List.length l) / 2 in
        let rec aux left right n = function
            | [] -> (List.rev left, List.rev right)
            | head :: tail -> if n >= midlen then aux left (head :: right) (n + 1) tail
                              else                aux (head :: left) right (n + 1) tail in
        aux [] [] 0 l in


    function
    | [] -> []
    | [n] -> [n]
    | numbers -> let left, right = halve numbers in
                    List.merge f (merge_sort f left) (merge_sort f right)


    in
        let sorted = merge_sort (fun a b -> a - b) input in
    begin
        List.iter (fun x -> Printf.printf "%d " x) sorted;
        print_newline ()
    end
;;
