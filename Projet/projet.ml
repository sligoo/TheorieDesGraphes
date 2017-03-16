open Dag;;
(*******************************************)
(*  Projet théorie des graphes 2016/2017   *)
(* Par Nicolas Surbayrole et Sacha Liguori *)
(*******************************************)

<<<<<<< Updated upstream
open Dag;;
=======

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

>>>>>>> Stashed changes

(* Question 6 *)
(* Fonction: Tri Topologique *)
(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnee selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme de tri topologique de l'enonce, en utilisant un format de file pour Y (section 1)
   *)

<<<<<<< Updated upstream
(* renvoie la liste des sommets sans prédécesseur *)
let source t = (Dag.fold_vertex (fun v l -> if ((Dag.pred t v) = []) then v::l else l) t []);;

(* print les sommets contenus dans une liste *)
let rec print_liste_vectex l = 
    match l with
      | [] -> ()
      | h::t -> ( (print_endline (Dag.namev h)); (print_liste_vectex t));;


(*let tri_topologique t = *)
	
=======
	
>>>>>>> Stashed changes
