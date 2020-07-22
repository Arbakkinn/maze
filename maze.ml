module Cell =
	struct
		type t = int * int
		let compare = Pervasives.compare
	end
;;

module CellMap = Map.Make(Cell);;
module CellSet = Set.Make(Cell);;
type grid = CellSet.t CellMap.t;;

(*
	range int -> int -> int list
	construit une liste de tous les entiers entre a et b inclus
	int -> int -> int list
*)
let rec range = fun a b ->
	if a <= b then
		a :: range (a+1) b	(* on ajoute a à la nouvelle liste et on recomence avec a+1 *)
	else
		[];;

(*
	construit une liste contenant tout les couples (i,j)
	avec 0 <= i < m et 0 <= j < n
	int -> int -> (int * int)
*)
let range2 = fun m n ->
	List.fold_left(	(* Premier fold pour i *)
		fun acc i ->
			List.fold_left(	(* Second fold pour j *)
				fun acc j ->
					(i, j)::acc		(* on ajout le couple suivit de l'acc *)
			) acc (range 0 (n-1))	(* ici l'acc dépend du fold précédant (range pour j) *)
	) [] (range 0 (m-1));;	(* ici l'acc est vide [] (range pour i) *)

(*
	renvoi l privée de son élément d'indice i
	(on suppose que i est > 0 et inf a la longueur de la liste)
	int list -> int -> int list
*)
let rec remove_nth = fun i l ->
	match l with
	| [] -> []
	| h::t ->
		if i = 0 then	(* si i = 0 alors on a finit et on renvoi la liste restante *)
			t
		else
			h :: remove_nth (i - 1) t;;	(* sinon on preserve h et on rapelle la fonction avec i-1 *)

(*
	renvoi le couple (x, r) où x est un élément de l choisi aléatoirement et où r est la liste l privée de x
	int list -> int * int list
*)
let extract_random = fun l ->
	let i = Random.int (List.length l) in	(* indice random *)
	((List.nth l i), (remove_nth i l));;	(* (ième élément de la liste, liste privée de l'élément à l'indice i) *)

(*
	Mélange une liste l
	int list -> int list
*)
let rec shuffle = fun l ->
	let n = List.length l in
	if n = 0 then	(* Si la liste est vide, on renvoi une liste vide *)
		[]
	else
		let i = Random.int n in
		(List.nth l i)::(shuffle (remove_nth i l));;	(* version condensé de l'algorithme 1 du poly *)

(*
	ajoute une case v à un graphe g seulement si g ne contient pas v
	Cell.t -> grid -> grid
*)
let add_vertex = fun v g ->
	if CellMap.mem v g then	(* Si le graphe contient déjà la case, on renvoi le graphe non modifié*)
		g
	else
		CellMap.add v CellSet.empty g;;	(* Sinon on ajoute une case v vide *)

(*
	ajoute une arrête de u à v
	Cell.t -> Cell.t -> grid -> grid
*)
let add_edge = fun u v g ->
	let cellSetU = CellMap.find u g in	(* contenu de la cellule u *)
	if CellSet.mem v cellSetU then	(* si v est successeur de u *)
		g
	else
		let newCellSetU = CellSet.add v cellSetU in	(* on ajoute v aux successeurs de u *)
		CellMap.add u newCellSetU g;;	(* On écrase l'ancien u *)

(*
	ajoute les arrêtes de u à v et de v à u
	Cell.t -> Cell.t -> grid -> grid
*)
let add_edges = fun u v g ->
	add_edge u v (add_edge v u g);;	(* arrête de v a u puis de u à v*)

(*
	return true si c correspond bien à une case de la grille de taille m * n, false sinon
	int -> int -> Cell.t -> bool
*)
let is_valid = fun m n c ->
	let x, y = c in (* Coordonées de la case *)
	if y >= m || y < 0 then	(* Erreur sur m *)
		false
	else if x >= n || x < 0 then (* Erreur sur n *)
		false
	else
		true;;

(*
	renvoie la liste constituée des quatres voisins potentiels de c
	Cell.t -> Cell.t list
*)
let get_neighbours = fun c ->
	let x, y = c in	(* Les coordonées de la case courante *)
	(x, y-1) :: (x+1, y) :: (x, y+1) :: (x-1, y) :: [];; (* Liste des potentiels voisins *)

(*
	Renvoie a liste des voisins de c
	int -> int -> Cell.t -> Cell.t list
*)
let get_valid_neighbours = fun m n c ->
	List.filter (fun x -> (is_valid m n x)) (get_neighbours c);; (* Test pour chaque voisin potentiel, si celui ci est dans la grille de taille m * n *)

(*
	Génère un graphe correspondant à une grille rectangulaire de taille m * n
	int -> int -> grid
*)
let create_grid = fun m n ->
	let lc = range2 n m in	(* Toutes les cases du graphe *)
	let g = (List.fold_left(
		fun acc c ->
			add_vertex c acc	(* On ajoute chaque case aux graphe *)
	) CellMap.empty lc) in

	List.fold_left(
		fun acc c ->
			List.fold_left(
				fun acc2 neighbour ->
					add_edge c neighbour acc2	(* On ajoute une arrête de c vers chacun de ses voisins *)
			) acc (get_valid_neighbours m n c) (* On récupère les voisins valide de la case *)
	) g lc;;

(*
	Retourne les coordonées du centre de la case c
	Cell.t -> int * int
*)
let get_center_coord = fun c ->
	let x, y = c in	(* Récupère les coordonées de la case *)
	(x * 2 + 1, y * 2 + 1);;	(* couple centre de la case *)

(*
	Retourne la liste des coordonées des quatres coins de la case c
	Cell.t -> (int * int) list
*)
let get_contour = fun c ->
	let x, y = c in	(* Coordonées de la case *)
	(*	List des contours de la case, dans l'ordre :
		- coin inférieur gauche
		- coin inférieur droit
		- coin supérieur droit
		- coin supérieur gauche
	*)
	(x * 2, y * 2) :: (x * 2 + 2, y * 2) :: (x * 2 + 2, y * 2 + 2) :: (x * 2, y * 2 + 2) :: [];;

(*
	Retourne la liste des coordonnées des coins communs aux case c1 et c2
	Cell.t -> Cell.t -> (int * int) list
*)
let get_wall = fun c1 c2 ->
	(* On récupère tout les coins commun entre les deux cases *)
	let l = List.filter (fun c_c1 -> (List.mem c_c1 (get_contour c2))) (get_contour c1) in
	if List.length l == 2 then	(* Si il y a deux points commun, il existe un mur *)
		l
	else
		[];;

(*
	Retourne un labyrinthe avec au minimum les mêmes murs que m
	grid -> grid > Cell.t -> Cell.t list -> grid
*)
let rec generate_maze_aux = fun g m v l ->
	match l with
	| [] -> m
	| v'::t ->
		if CellMap.mem v' m then	(* Si v' est déjà un sommet de m, on renvoi m *)
			generate_maze_aux g m v t
		else
			let m' = add_edges v v' (add_vertex v' m) in
			(* Ne pas oublier de retirer v de g dans les successeurs de v' *)
			let l' = shuffle (CellSet.elements (CellSet.remove v (CellMap.find v' g))) in
			let m'' = generate_maze_aux g m' v' l' in
			generate_maze_aux g m'' v t;;

(*
	Retourne un labyrinthe sur une grille m donnée en argument
	grid -> grid
*)
let generate_maze = fun g ->
	let c,v = CellMap.choose g in
	generate_maze_aux g (add_vertex (0,0) CellMap.empty) c (CellSet.elements (CellMap.find c g));;

(*
	Renvoi true si e apparaît dans l, false sinon
*)
let rec find = fun l e ->
    match l with
	| [] -> false
	| h::t ->
	    h = e || find t e;;

(*
	Renvoie une liste l sans les doublons
*)
let rec nub = fun l ->
    match l with
	| [] -> []
	| h::t ->
	    if find t h then	(* si h est dans la suite de la liste on ne l'ajoute pas (on l'ajoutera plus tard) *)
			nub t
	    else			(* sinon on l'ajoute a la liste de retour *)
			h :: nub t;;


(*
	s : ensemble de sommets déjà visités
	grid -> Cell.t -> Cell.t -> Cell.t list ->
*)
let rec solve_maze_aux = fun g u v l s ->
	if u = v then	(* On atteint la case d'arrivé *)
		v :: []
	else
		match l with
		| [] -> [] (* Si il n'y a pas/plus de successeurs pour le sommet courant = chemin pas bon *)
		| u' :: t ->
			if CellSet.mem u' s then (* Si on a déjà visité le sommet u' *)
				solve_maze_aux g u v t s (* On recommence avec le reste de la liste *)
			else
				let s' = CellSet.add u' s in (* On ajoute u' à la liste des sommets déjà visités *)
				let l' = CellSet.elements (CellMap.find u' g) in (* On récupère les successeurs de u' *)
				let path_u = solve_maze_aux g u v t s' in	(* On essaye le chemin avec u *)
				if path_u = [] then
					let path_u' = solve_maze_aux g u' v l' s' in	(* On essaye le chemin avec u' *)
					if path_u' = [] then
						[]
					else
						u' :: path_u'	(* Si le chemin retourné par u' n'est pas vide, c'est que u' est bon *)
				else
					u :: path_u;; (* Idem qu'au dessus pour u *)

(*
	Résout un labyrinthe, en paramètre : le labyrinthe, la case d'entrée et la case de sortie
	grid -> Cell.t -> Cell.t -> Cell.t list
*)
let solve_maze = fun g u v ->
	let l = CellSet.elements (CellMap.find u g) in	(* Liste des successeurs de u *)
	let s = CellSet.singleton u in	(* au début on a visité juste la case d'entrée *)
	u :: (nub (solve_maze_aux g u v l s));;	(* La liste contenant le chemin (sans les éventuelles doublons) *)


#use "display.ml" ;;
Random.self_init();;
let g = create_grid 100 120;;
let m = generate_maze g;;
let l = solve_maze m (0,0) (89,69) in
test (1000,800) (4,10,10) m (0,0) (89,69) l;;
