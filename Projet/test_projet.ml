#use "dag_test.ml";;
#use "projet.ml";;

let nb_ressource_multi = 2 in
let nb_ressource1 = 2 in
let nb_ressource2 = 2 in
let alpha = 2.0 in

(* test ordonnancement séquentiel *)

print_endline "=== test ordonnancement séquentiel ===";
print_endline "";

print_endline "== Dag 1 ==";
print_liste_vectex (tri_topologique dag1);
print_endline "";

print_endline "== Dag 2 ==";
print_liste_vectex (tri_topologique dag2);
print_endline "";

print_endline "== Dag 3 ==";
print_liste_vectex (tri_topologique dag3);
print_endline "";

print_endline "== Dag 4 ==";
print_liste_vectex (tri_topologique dag4);
print_endline "";


print_endline "";
print_endline ("=== test ordonnancement multi avec "^(string_of_int nb_ressource_multi)^" ===");
print_endline "";

print_endline "== Dag 1 ==";
print_trace (ordonnanceur_multi nb_ressource_multi dag1);
print_endline "";

print_endline "== Dag 2 ==";
print_trace (ordonnanceur_multi nb_ressource_multi dag2);
print_endline "";

print_endline "== Dag 3 ==";
print_trace (ordonnanceur_multi nb_ressource_multi dag3);
print_endline "";

print_endline "== Dag 4 ==";
print_trace (ordonnanceur_multi nb_ressource_multi dag4);
print_endline "";

print_endline "";
print_endline ("=== test ordonnancement multi avec 2 types de ressources et distribution aléatoire ===");
print_endline ("nombre ressource de type 1 : "^(string_of_int nb_ressource1));
print_endline ("nombre ressource de type 2 : "^(string_of_int nb_ressource2));
print_endline ("hétérogénéité : "^(string_of_float alpha));
print_endline "";

print_endline "== Dag 1 ==";
print_trace (ordonnanceur_heterogene alpha nb_ressource1 nb_ressource2 dag1);
print_endline "";

print_endline "== Dag 2 ==";
print_trace (ordonnanceur_heterogene alpha nb_ressource1 nb_ressource2 dag2);
print_endline "";

print_endline "== Dag 3 ==";
print_trace (ordonnanceur_heterogene alpha nb_ressource1 nb_ressource2 dag3);
print_endline "";

print_endline "== Dag 4 ==";
print_trace (ordonnanceur_heterogene alpha nb_ressource1 nb_ressource2 dag4);
print_endline "";

print_endline "";
print_endline ("=== test ordonnancement multi avec 2 types de ressources et distribution intelligente ===");
print_endline ("nombre ressource de type 1 : "^(string_of_int nb_ressource1));
print_endline ("nombre ressource de type 2 : "^(string_of_int nb_ressource2));
print_endline ("hétérogénéité : "^(string_of_float alpha));
print_endline "";

print_endline "== Dag 1 ==";
print_trace (ordonnanceur_heterogene_quick alpha nb_ressource1 nb_ressource2 dag1);
print_endline "";

print_endline "== Dag 2 ==";
print_trace (ordonnanceur_heterogene_quick alpha nb_ressource1 nb_ressource2 dag2);
print_endline "";

print_endline "== Dag 3 ==";
print_trace (ordonnanceur_heterogene_quick alpha nb_ressource1 nb_ressource2 dag3);
print_endline "";

print_endline "== Dag 4 ==";
print_trace (ordonnanceur_heterogene_quick alpha nb_ressource1 nb_ressource2 dag4);
print_endline "";


();;
