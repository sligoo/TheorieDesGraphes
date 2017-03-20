(*******************************************)
(*  Projet théorie des graphes 2016/2017   *)
(* Par Nicolas Surbayrole et Sacha Liguori *)
(*******************************************)

open Dag;;
open Queue;;
open List;;

(************************************************************
 ******************** Question 6 ****************************
 ************************************************************)

(* print les sommets contenus dans une liste *)
let rec print_liste_vectex l = 
    match l with
      | [] -> (print_endline " ")
      | h::t -> ( (print_string ((Dag.namev h) ^":"^(string_of_float (Dag.costv h) )^" ")); (print_liste_vectex t));;

(* print une trace *)
let rec print_trace l =
    match l with
      | [] -> (print_endline " ")
      | h::t -> ( (print_liste_vectex h); (print_trace t));;

(* renvoie si les éléments de l1 sont contenus dans l2 (l1 l2 non trié) (ne vérifie pas le multiplicité) *)
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

(* renvoie la queue des sommets sans prédécesseur *)
let source t = (Dag.fold_vertex (fun v l -> (if ((Dag.pred t v) = []) then (Queue.add v l) ); l) t (Queue.create ()));;

let rec addAvertex graphe y z =
    if (Queue.is_empty y) then z else
        let h = (Queue.take y) in  
        let z2 = h::z in 
        addAvertex graphe (predInZ graphe (Dag.succ graphe h) y z2) z2;;
              

(* Fonction: Tri Topologique *)
(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnee selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme de tri topologique de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
let tri_topologique t = List.rev (addAvertex t (source t) []);;


(************************************************************
 ******************** Question 8 ****************************
 ************************************************************)


let listunique l =
    let rec listuniquerec l1 l2 =
        (match l1 with
           | [] -> l2
           | h::t -> if (List.mem h l2) then (listuniquerec t l2) else (listuniquerec t (h::l2)) )
    in listuniquerec l [];;

let rec addvertexs n y = (* retourne une liste de n processus disponible *)
    if ((n = 0) || (Queue.is_empty y)) then [] else (let h =(Queue.take y) in (h::(addvertexs (n-1) y ) ) );;

let rec addNvertexs graphe n y z term nterm =
    if (Queue.is_empty y && nterm = []) then z else
        let verts = (List.append nterm (addvertexs (n-(List.length nterm)) y)) in (* liste des vertexs qui s'éxécute ce tour ci *)
        let z2 = verts::z in (* trace d'éxecution *)
        let newterm = ((List.iter (fun a -> (Dag.decreasev a 1.0)) verts); (* calcul du temps restant *)
                       (fold_right (fun a l -> if (Dag.computedv a) then a::l else l) verts [])) in (* nouveaux nœuds terminaux *)
        let nterm2 = (fold_right (fun a l -> if (Dag.computedv a) then l else a::l) verts []) in (* nœuds non terminaux éxécutés *)
        let term2 = (List.append newterm term) in (* ensemble des noeuds terminés *)
        let successeurs = (listunique (fold_right (fun a l -> (List.append l (Dag.succ graphe a)) ) newterm [])) in
        addNvertexs graphe n (predInZ graphe successeurs y term2) z2 term2 nterm2;;

(* entrees:
   - facteur alpha de vitesse relative (section 4)
   - un nombre entier de ressources de type 1 r1
   - un nombre entier de ressources de type 2 r2
   - un DAG
   sorties:
   - une trace d'execution du DAG
   specifs: 
   - vous prendrez en compte le type de ressource, de sorte à donner une trace valide
   - vous n'utiliserez pas d'heuristique
   *)
let ordonnanceur_multi n graphe = (Dag.init graphe); List.rev (addNvertexs graphe n (source graphe) [] [] []);;

(************************************************************
 ******************** Question 12 ***************************
 ************************************************************)

let rec addVertexres1 graphe nres1 nres2 alpha y z term ntermres1 ntermres2 =
    if (Queue.is_empty y && ntermres1 = [] && ntermres2 = []) then z else
        let verts1 = (List.append ntermres1 (addvertexs (nres1-(List.length ntermres1)) y)) in (* liste des vertexs qui s'éxécute ce tour ci sur des ressources 1*)
        let verts2 = (List.append ntermres2 (addvertexs (nres2-(List.length ntermres2)) y)) in (* liste des vertexs qui s'éxécute ce tour ci sur des ressources 2*)
        let z2 = (List.append verts1 verts2)::z in (* trace d'éxecution *)
        let newterm = (* calcul du temps restant *)
            ((List.iter (fun a -> (Dag.decreasev a (if (Dag.typeressv a = 1) then 1.0 else (1.0/.alpha )))) verts1);
             (List.iter (fun a -> (Dag.decreasev a (if (Dag.typeressv a = 2) then 1.0 else (1.0/.alpha )))) verts2);
             (fold_right (fun a l -> if (Dag.computedv a) then a::l else l) (List.append verts1 verts2) [])) in (* nouveaux nœuds terminaux *)
        let nterm1 = (fold_right (fun a l -> if (Dag.computedv a) then l else a::l) verts1 []) in (* nœuds non terminaux éxécutés *)
        let nterm2 = (fold_right (fun a l -> if (Dag.computedv a) then l else a::l) verts2 []) in (* nœuds non terminaux éxécutés *)
        let term2 = (List.append newterm term) in (* ensemble des noeuds terminés *)
        let successeurs = (listunique (fold_right (fun a l -> (List.append l (Dag.succ graphe a)) ) newterm [])) in
        addVertexres1 graphe nres1 nres2 alpha (predInZ graphe successeurs y term2) z2 term2 nterm1 nterm2;;

(* entrees:
   - facteur alpha de vitesse relative (section 4)
   - un nombre entier de ressources de type 1 r1
   - un nombre entier de ressources de type 2 r2
   - un DAG
   sorties:
   - une trace d'execution du DAG
   specifs: 
   - vous prendrez en compte le type de ressource, de sorte à donner une trace valide
   - vous n'utiliserez pas d'heuristique
   *)
let ordonnanceur_heterogene alpha nres1 nres2 graphe = (Dag.init graphe); List.rev (addVertexres1 graphe nres1 nres2 alpha (source graphe) [] [] [] []);;


(************************************************************
 ******************** Question 14 ***************************
 ************************************************************)

(* renvoie la liste des sommets sans prédécesseur *)
let source_list t = (Dag.fold_vertex (fun v l -> (if ((Dag.pred t v) = []) then v::l else l)) t []);;


(* ajoute les ressources d'une liste dans la queue correspondant *)
let rec divise_ressource l q1 q2 =
    match l with
      | [] -> ()
      | h::t -> ( (if (Dag.typeressv h = 1) then (Queue.add h q1) else (Queue.add h q2)); divise_ressource t q1 q2);; 


let rec predInZ_list graphe noeuds z =
    (fold_right (fun a l -> (if (list_in_list (Dag.pred graphe a) z) then a::l else l)) noeuds []);;

(* param :
    * graphe : le Dag
    * nres1 : nombre de ressource de type 1
    * nres2 : nombre de ressource de type 2
    * alpha : facteur d'hétérogénéité
    * y1 : file des processus de type 1 disponible non commencé
    * y2 : file des processus de type 2 disponible non commencé
    * z : trace d'éxécution (qui sera incrémenté)
    * term : liste des processus terminés
    * ntermres1 : liste des processus commencé non terminé de type 1
    * ntermres2 : liste des processus commencé non terminé de type 2
  result :
      la trace d'éxecution dans l'ordre anti chronologique
*)
let rec addVertexres2 graphe nres1 nres2 alpha y1 y2 z term ntermres1 ntermres2 =
    if (Queue.is_empty y1 && Queue.is_empty y2 && ntermres1 = [] && ntermres2 = []) then z else
        (* affectation de nouveaux processus dans les ressources en fonction des ressources *)
        let tmp_verts1 = (List.append ntermres1 (addvertexs (nres1-(List.length ntermres1)) y1)) in 
        let tmp_verts2 = (List.append ntermres2 (addvertexs (nres2-(List.length ntermres2)) y2)) in 
        (* affectation de nouveaux processus dans les ressources sans distinction de ressources si il reste des ressources disponible *)
        let verts1 = (List.append tmp_verts1 (addvertexs (nres1-(List.length tmp_verts1)) y1)) in 
        let verts2 = (List.append tmp_verts2 (addvertexs (nres2-(List.length tmp_verts2)) y2)) in 
        (* ajout de la trace d'éxecution *)
        let z2 = (List.append verts1 verts2)::z in
        (* Calcul du temps restant des processus et des processus finis *)
        let newterm = 
            ((List.iter (fun a -> (Dag.decreasev a (if (Dag.typeressv a = 1) then 1.0 else (1.0/.alpha )))) verts1);
             (List.iter (fun a -> (Dag.decreasev a (if (Dag.typeressv a = 2) then 1.0 else (1.0/.alpha )))) verts2);
             (fold_right (fun a l -> if (Dag.computedv a) then a::l else l) (List.append verts1 verts2) [])) in 
        let term2 = (List.append newterm term) in (* ensemble des noeuds terminés *)
        (* calcul des processus non terminés (qui continuront au temps t+1) *)
        let nterm1 = (fold_right (fun a l -> if (Dag.computedv a) then l else a::l) verts1 []) in
        let nterm2 = (fold_right (fun a l -> if (Dag.computedv a) then l else a::l) verts2 []) in
        (* calcul des nouveaux processus disponibles *)
        let successeurs = (listunique (fold_right (fun a l -> (List.append l (Dag.succ graphe a)) ) newterm [])) in
        (* Ajout des nouveaux processus disponibles dans la bonne file *)
        ( divise_ressource (predInZ_list graphe successeurs term2) y1 y2;
        (* Récursion *)
        addVertexres2 graphe nres1 nres2 alpha y1 y2 z2 term2 nterm1 nterm2);;

(* entrees: 
   - facteur alpha de vitesse relative (section 4)
   - un nombre entier de ressources de type 1 r1
   - un nombre entier de ressources de type 2 r2
   - un DAG
   sorties:
   - une trace d'execution du DAG
   specifs: 
   - vous prendrez en compte le type de ressource, de sorte à donner une trace valide
   - vous ameliorerez de manière simple l'algorithme de facon a reduire le temps total d'execution
   - vous n'utiliserez pas d'heuristique
   *)

let ordonnanceur_heterogene_quick alpha nres1 nres2 graphe = (Dag.init graphe); 
            let q1 = Queue.create () in
            let q2 = Queue.create () in
            (divise_ressource (source_list graphe) q1 q2;
             List.rev (addVertexres2 graphe nres1 nres2 alpha q1 q2 [] [] [] []) );;
