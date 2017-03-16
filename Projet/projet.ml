open Dag;;
(*******************************************)
(*  Projet théorie des graphes 2016/2017   *)
(* Par Nicolas Surbayrole et Sacha Liguori *)
(*******************************************)

(* print les sommets contenus dans une liste *)
let rec print_liste_vectex l = 
    match l with
      | [] -> ()
      | h::t -> ( (print_endline (Dag.namev h)); (print_liste_vectex t));;

let rec app e l =
  match l with
  |[] -> false
  |(e', _) :: tl -> e = e' || app e tl
;;
 
let rec inclus l1 l2 =
  match l1, l2 with
  | [], _ -> true
  | _, [] -> false
  | (a, _)::tl1, l2 -> app a l2 && inclus tl1 l2
;;

(* predInZ *)
(* Ajouter a la liste y,les noeuds se trouvant dans la liste *)
(* et dont les predecesseurs sont dans Z *)
(* Parametres: 
	-graphe: graphe étudié 
	-noeuds: liste des noeuds 
	-y: la liste dans laquelle ajouter les noeuds 
	-z: la liste dans laquelle les successeurs doivent etre
*)
let rec predInZ graphe noeuds y z =
	match noeuds with
		| [] -> y
		| t::q -> if (inclus (pred graphe t) z) then predInZ graphe q (t::y) z
  				else predInZ graphe q y z ;;	

(* renvoie la liste des sommets sans prédécesseur *)
let source t = (Dag.fold_vertex (fun v l -> if ((Dag.pred t v) = []) then v::l else l) t []);;



(* Question 6 *)
(* Fonction: Tri Topologique *)
(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnee selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme de tri topologique de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
(*let tri_topologique t = *)
