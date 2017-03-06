let l1 = [5; 4; 3; 9; 25];;
let l2 = [98; 55; 45; 12];;

let rec minListe liste =
	match liste with	
	|[t] -> t
	|t::q -> min t (minListe q)
	|_ -> failwith("liste vide") ;;

let minListeFold liste = 
	match liste with	
	|[t] -> t
	|t::q -> List.fold_right min q t
	|_ -> failwith("liste vide") ;;

let rec occuUn liste val1 = 
	match liste with
	|[(a,b)] -> if a = val1 then 1 else 0
	|(a,b)::q -> if a = val1 then (1 + occuUn q val1) else occuUn q val1
	| _ -> failwith("liste vide") ;;