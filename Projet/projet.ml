open Dag;;
open Queue;;
open List;;
open Set;;
(*******************************************)
(*  Projet théorie des graphes 2016/2017   *)
(* Par Nicolas Surbayrole et Sacha Liguori *)
(*******************************************)

(* print les sommets contenus dans une liste *)
let rec print_liste_vectex l = 
    match l with
      | [] -> (print_endline " ")
      | h::t -> ( (print_string ((Dag.namev h) ^" ")); (print_liste_vectex t));;

let rec list_in_list l1 l2 = 
    match (l1, l2) with
      | [], _ -> true
      | _, [] -> false
      | h1::t1, h2::t2 -> if (h1 = h2) 
                          then (list_in_list t1 t2)
                          else ((list_in_list ([h1]) t2) && (list_in_list t1 l2));;
                                        

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
        | t::q -> (if (list_in_list (Dag.pred graphe t) z) then (Queue.add t y) ); predInZ graphe q y z ;;	

(* renvoie la liste des sommets sans prédécesseur *)
let source t = (Dag.fold_vertex (fun v l -> (if ((Dag.pred t v) = []) then (Queue.add v l) ); l) t (Queue.create ()));;

let rec addAvertex graphe y z =
    if (Queue.is_empty y) then z else
        let h = (Queue.take y) in  
        let z2 = h::z in 
        addAvertex graphe (predInZ graphe (Dag.succ graphe h) y z2) z2;;
              

(* Question 6 *)
(* Fonction: Tri Topologique *)
(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnee selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme de tri topologique de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
let tri_topologique t = List.rev (addAvertex t (source t) []);;

let listunique l =
    let rec listuniquerec l1 l2 =
        (match l1 with
           | [] -> l2
           | h::t -> if (List.mem h l2) then (listuniquerec t l2) else (listuniquerec t (h::l2)) )
    in listuniquerec l [];;

let rec addNvertexs graphe n y z =
    let rec addvertexs n y l = (
        if ((n = 0) || (Queue.is_empty y)) then l else addvertexs (n-1) y ((Queue.take y)::l)) in
    if (Queue.is_empty y) then z else
        let verts = (addvertexs n y []) in
        let z2 = verts::z in
        let successeurs = (listunique (fold_right (fun a l -> (List.append l (Dag.succ graphe a)) ) verts [])) in
        addNvertexs graphe n (predInZ graphe successeurs y (List.flatten z2)) z2;;

let ordonnanceur_multi n graphe = List.rev (addNvertexs graphe n (source graphe) []);;

    
