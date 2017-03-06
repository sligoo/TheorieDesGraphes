module G = Graph.Pack.Graph;;
module V = G.V;;
module M = G.Mark;;

let even (x : int) = (x mod 2 = 0 : bool);;

let est_degre_pair graphe sommet = 
	even (out_degree graphe sommet)
;;

let sommets_impairs g = 
	G.fold_vertex (fun v l -> if est_degre_pair g v then l else v::l) g []
;;

let sommet_lambda graphe = 
	G.fold_vertex (fun sommet _ -> Some sommet ) graphe None
;;

let rec marquage graphe sommet =
	match M.get sommet with
	|0 ->
		begin
			M.set sommet 1;
			G.iter_succ (marquage graphe) graphe sommet
		end
	|_ -> ()
;;


let est_connexe graphe = 
	match sommet_lambda graphe with
	|None -> true
	|Some sommet -> 
		begin
			M.clear graphe;
			marquage graphe sommet;
			G.fold_vertex (fun sommet c -> c && M.get sommet = 1) graphe true
		end
;;


let est_eulerien graphe = 
	match est_connexe graphe with
    | false -> false
    | true -> 
    match List.length (sommets_impairs graphe) with
        | 0 -> true
        | _ -> false
;;

let est_semi_eulerien graphe = 
	match est_connexe graphe with
    | false -> false
    | true -> match List.length (sommets_impairs graphe) with
        | 2 -> true
        | _ -> false
;;


let rec chemin g a b =
    if a = b then
        [a]
    else
        match G.succ g a with
            | [] -> failwith "Pas Eulerien"
            | h::t ->
                begin
                    G.remove_edge g a h;
                    a::(chemin g h b);
                end
;;

let cycle g v =
    match G.succ g v with
        | [] -> failwith "Pas Eulerien"
        | h::t ->
            begin
                G.remove_edge g v h;
                v::(chemin g h v);
            end
;;

let rec completion g = function
    | [] -> []
    | h::t ->
        if G.out_degree g h = 0 then h::(completion g t)
        else completion g (cycle g h)@t
;;

let cycle_eulerien graphe = 
	match sommet_lambda graphe with
	| None -> []
	| Some sommet -> completion graphe (cycle graphe sommet)
;;