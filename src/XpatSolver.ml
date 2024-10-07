open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }

(* Règle de couleur en fonction de la game*)
type couleur = Pareil | Alterne | Importe

(* Règle au niveau des colonnes vides *)
type colonnevide = Tout | Roi | Rien

(* Ensembles de règles composés de la taille des colonnes, du registre, couleurs et placements autorisés *)
type regles = { mutable t1 : int; mutable t2 : int; mutable colonne : int; mutable reg : int; mutable couleur : couleur; mutable colonnevide : colonnevide }
let regles = { t1 = 7; t2 = 6; colonne = 8; reg = 4; couleur = Alterne; colonnevide = Tout }

(* Dépot représentant le nombre de cartes correspondant à un motif rangées *)
type depot = { coeur : int ; trefle : int ; pique : int ; carreau : int }

(* Registre contenant les cartes stockés temporairement *)
type registre = Card.card list

(* Etat qui contient les colonnes, le depot et le registre *)
type etat = { mutable colonnes : (Card.card list) PArray.t ; mutable depot : depot ; mutable registre : registre}
let etat = {colonnes = PArray.init regles.colonne (fun _ -> []) ; depot = {coeur = 0; trefle = 0; pique = 0; carreau = 0} ; registre = []}

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

(* Initialisation des règles en fonction de la game *)
let set_regles = function
  | Freecell -> regles.t1 <- 7; regles.t2 <- 6; regles.colonne <- 8; regles.reg <- 4; regles.couleur <- Alterne; regles.colonnevide <- Tout 
  | Seahaven -> regles.t1 <- 5; regles.t2 <- 5; regles.colonne <- 10; regles.reg <- 4; regles.couleur <- Pareil; regles.colonnevide <- Roi
  | Midnight -> regles.t1 <- 3; regles.t2 <- 3; regles.colonne <- 18; regles.reg <- 0; regles.couleur <- Pareil; regles.colonnevide <- Rien 
  | Baker -> regles.t1 <- 4; regles.t2 <- 4; regles.colonne <- 13; regles.reg <- 0; regles.couleur <- Importe; regles.colonnevide <- Rien

let set_colonnes colonnes = etat.colonnes <- colonnes 
let set_registre registre = etat.registre <- registre

(* Initialisation des registres *)
let remplissage_registre name permut = 
  match name with
  | Freecell -> []
  | Seahaven -> [(Card.of_num (List.nth permut 50));(Card.of_num (List.nth permut 51))]
  | _ -> [] 

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^ "FreeCell Seahaven MidnightOil BakersDozen")

(* Traitement du cas BakerDozen où un roi doit être placé en haut d'une colonne au début de la partie *)

let regle_bakers colonne = 
  let rec remonter_roi roi nonroi col = 
    match col with
    (* si c'est un roi on le met dans la liste roi , sinon dans nonroi puis à la fin on concatène les liste ensemble *)
    | [] -> roi @ nonroi
    | c :: r -> if fst c = 13 then remonter_roi (c :: roi) nonroi r else remonter_roi roi (nonroi @ [c]) r
  in remonter_roi [] [] colonne

let remonter_roi_colonnes regles colonnes = 
  if (regles.colonne = 13) then PArray.map regle_bakers colonnes else colonnes

(* Deux étapes lors de la construction de colonnes : *)
  
(* Construit une liste et y place des cartes en respectant la taille demandé *)
let rec construction_liste taille (t : int) permut (i : int) n = 
  if (i = 51 && n = 17) then [Card.of_num (List.nth permut 51)] (* cas MidnightOil *) else 
  if (taille = t) then [] else (Card.of_num (List.nth permut i) :: (construction_liste taille (t+1) permut (i+1) n)) 

(* Contruit un ensemble de colonnes *)
let construction_partie regles permut = 
  let rec tmp (n : int) (i : int) colonnes = 
    if (n = regles.colonne) then (remonter_roi_colonnes regles colonnes)
    else 
      (* on alterne entre la taille t1 et t2 des règles définis *)
      match (n mod 2) with 
      | 0 -> tmp (n+1) (i+regles.t1) (PArray.set colonnes n (construction_liste regles.t1 0 permut i n))
      | _ -> tmp (n+1) (i+regles.t2) (PArray.set colonnes n (construction_liste regles.t2 0 permut i n))
  in tmp 0 0 (PArray.init regles.colonne (fun _ -> []))

(* Ensemble de fonctions d'affichages *)

let affichage_colonnes colonnes = 
  let rec affiche colonnes i =
    (if (i = PArray.length colonnes) then print_string "\n" else (Printf.printf "\nCOLONNE %d : " i;
      List.iter (fun n -> Printf.printf "%s " (Card.to_string n)) (PArray.get colonnes i);
      (affiche colonnes (i+1))))
  in affiche colonnes 0

let affichage_registre registre = 
  print_string "REGISTRE : ";
  match registre with
  | [] -> print_string "vide\n";
  | _ -> List.iter (fun n -> Printf.printf "%s " (Card.to_string n)) registre

let affichage_depot depot = 
  print_string "\nDEPOT : ";
  (Printf.printf "Coeur: %d " depot.coeur);
  (Printf.printf "Pique: %d " depot.pique);
  (Printf.printf "Trefle: %d " depot.trefle);
  (Printf.printf "Carreau: %d\n" depot.carreau)

(* Fonctions qui parcourent un(e) colonne/registre *)

(* renvoie le numéro de la dernière carte d'une colonne *)
let last_card_colonne colonne = 
  match colonne with
  | [] -> -1 
  | _ -> (Card.to_num (List.nth colonne ((List.length colonne) - 1)))

(* vérifie si la colonne est vide *)
let trouver_colonne_vide colonne = 
  match colonne with
  | [] -> true
  | _ -> false

(* renvoie l'indice de la première colonne vide si elle existe *)
let trouver_colonnes_vides colonnes = 
  let rec trouver i = 
    if (i = PArray.length etat.colonnes) then -1 else
    (if (trouver_colonne_vide (PArray.get etat.colonnes i)) then i else (trouver (i+1)))
  in trouver 0 

(* enlève la dernière carte d'une colonne *)
let rec remove_last_card colonne = 
  match colonne with
  | [] -> []
  | [x] -> []
  | v :: l' -> v :: remove_last_card l'

(* vérfie qu'il existe une colonne qui contient en dernière carte celle qu'on recherche et renvoie son indice *)
let verif_colonne n = 
  let rec verif i = 
    if (i = PArray.length etat.colonnes) then (-1) else
    (if (last_card_colonne (PArray.get etat.colonnes i) = n) then i else verif (i+1))
  in verif 0
  
(* pareil mais avec le registre *)
let verif_registre s1 registre = 
  if (regles.reg = 0) then -1 else 
  let rec parcours_registre s1 registre i = 
    match registre with
    | [] -> -1 
    | v :: l' -> if ((Card.to_num v) = s1) then i else parcours_registre s1 l' (i+1)
  in parcours_registre s1 registre 0

(* Fonctions qui traitent et/ou placent des cartes à des emplacements précis *)

(* ajoute une carte à la fin d'une liste *)
let append_card liste card = liste @ [card]

(* enlève une carte du registre *)
let rec remove_registre registre i = 
  match registre with
  | [] -> []
  | (a,b) :: l' ->  if (a = (fst (List.nth etat.registre i)) && b = (snd (List.nth etat.registre i))) then 
    (remove_registre l' i) else ((a,b) :: remove_registre l' i)

(* place une carte dans une colonne vide *)
let action_V i = 
  let indice = trouver_colonnes_vides etat.colonnes in 
    if (indice = -1) then false
    else 
      let colonne = PArray.get etat.colonnes i in 
      let carte = List.nth colonne ((List.length colonne)-1) in 
      set_colonnes (PArray.set etat.colonnes indice [carte]) ; 
      set_colonnes (PArray.set etat.colonnes i (remove_last_card (colonne))) ; true

(* ajoute une carte dans le registre *)
let action_T i = 
  if (regles.reg = 0 || List.length (etat.registre) = 4) then false
  else 
    let colonne = PArray.get etat.colonnes i in 
    let carte = List.nth colonne ((List.length colonne)-1) in 
    set_registre (append_card (etat.registre) carte) ;
    set_colonnes (PArray.set etat.colonnes i (remove_last_card (colonne))) ; true
    
(* récupère la carte d'une colonne à l'indice i *)
let get_card_colonne i = 
  let colonne = PArray.get (etat.colonnes) i in
  let carte = List.nth colonne ((List.length colonne)-1) in carte

(* récupère la carte d'un registre à l'indice i *)
let get_card_registre i = List.nth etat.registre i

(* verifie que les règles concernant les colonnes vides sont respectés *)
let verif_droit_colonne_vide carte = 
  match (regles.colonnevide) with
  | Tout -> true
  | Roi -> fst carte = 13
  | Rien -> false

(* verifie que les règles concernant les couleurs des cartes sont respectés *)
let verif_couleur_carte carte1 carte2 = 
  match (regles.couleur) with 
  | Pareil -> Card.get_couleur_carte carte1 = Card.get_couleur_carte carte2
  | Alterne -> not (Card.get_couleur_carte carte1 = Card.get_couleur_carte carte2)
  | Importe -> true

(* vérifie que la carte que l'on veut placer est bien immédiatement inférieure à la carte de destination *)
let verif_cartes_successives carte1 carte2 = (fst carte1) + 1 = (fst carte2)

(* déplacements de cartes colonne -> colonne *)
let transfer_colonne i indice = 
  let colonne_i = PArray.get etat.colonnes i in 
  let colonne_indice = PArray.get etat.colonnes indice in 
  let carte = List.nth colonne_i ((List.length colonne_i) -1) in 
  set_colonnes (PArray.set etat.colonnes indice (append_card colonne_indice carte)) ;
  set_colonnes (PArray.set etat.colonnes i (remove_last_card colonne_i)) ; true

(* vérifie que le déplacement de colonnes est autorisé *)
let verif_deplacement_colonne i indice = 
  let colonne_i = PArray.get etat.colonnes i in
  let colonne_indice = PArray.get etat.colonnes indice in
  let carte_i = List.nth colonne_i ((List.length colonne_i)-1) in
  let carte_indice = List.nth colonne_indice ((List.length colonne_indice)-1) in
  if ((verif_couleur_carte carte_i carte_indice) && (verif_cartes_successives carte_i carte_indice)) then transfer_colonne i indice else false

(* vérifie qu'il existe une colonne contenant la carte s2 avant de faire le déplacement *)
let action_N i s2 = 
  let ind_c = verif_colonne s2 in 
  if (ind_c = -1) then false else verif_deplacement_colonne i ind_c
      
(* traite l'action de s2 *)
let traitement_colonnes i s2 = 
  match s2 with
  | "V" -> if (verif_droit_colonne_vide (get_card_colonne i)) then action_V i else false
  | "T" -> action_T i
  | _ -> action_N i (int_of_string s2)

let action_V_r i = 
  let indice = trouver_colonnes_vides etat.colonnes in  
  if (indice = -1) then false
  else (set_colonnes (PArray.set (etat.colonnes) indice [List.nth (etat.registre) i]) ;
  set_registre (remove_registre (etat.registre) i) ; true)
      
let action_T_r i = 
  if (regles.reg = 0 || List.length (etat.registre) = 4) then false
  else 
    let carte = (List.nth (etat.registre) i) in
    set_registre (remove_registre (etat.registre) i) ;
    set_registre (append_card (etat.registre) carte ); true

(* déplacements de cartes registre -> colonne *)
let transfer_registre i indice = 
  set_colonnes (PArray.set (etat.colonnes) indice (append_card (PArray.get (etat.colonnes) indice) (List.nth etat.registre i)));
  set_registre (remove_registre etat.registre i); true

(* vérification des droits avant le déplacement de cartes  *)
let verif_deplacement_registre i indice = 
  let carte_registre = (List.nth etat.registre i) in 
  let colonne = PArray.get etat.colonnes indice in
  let carte_colonne = (List.nth colonne ((List.length colonne)-1)) in
  if ((verif_couleur_carte carte_registre carte_colonne) && (verif_cartes_successives carte_registre carte_colonne)) then transfer_registre i indice else false

let action_N_r i s2 = 
  let ind_c = verif_colonne s2 in 
  if (ind_c = -1) then false else verif_deplacement_registre i ind_c

(* traite la deuxième composante de la ligne et agit en conséquence *)
let traitement_registre i s2 =
  match s2 with
  | "V" -> if (verif_droit_colonne_vide (get_card_registre i)) then action_V_r i else false
  | "T" -> action_T_r i 
  | _ -> action_N_r i (int_of_string s2)

(* cherche où se trouve la carte s1, et vérifie qu'elle soit déplacable*)
let traitement_composante1 s1 s2 = 
  let ind_c = verif_colonne s1 in 
  let ind_r = verif_registre s1 (etat.registre) in 
  if (ind_c = -1) then (if (ind_r = -1) then false else traitement_registre ind_r s2) else traitement_colonnes ind_c s2

(* Fonctions qui gèrent le dépot *)

(* vérifie si l'on a gagné la partie *)
let verif_gagnant depot = depot.coeur = 13 && depot.pique = 13 && depot.trefle = 13 && depot.carreau = 13

(* met à jour le dépot *)
let ajout_depot_val motif = 
  match motif with
  | "Co" -> etat.depot <- {coeur = etat.depot.coeur +1; trefle = etat.depot.trefle; pique = etat.depot.pique; carreau = etat.depot.carreau}; ()
  | "Pi" -> etat.depot <- {coeur = etat.depot.coeur; trefle = etat.depot.trefle; pique = etat.depot.pique +1; carreau = etat.depot.carreau}; ()
  | "Ca" -> etat.depot <- {coeur = etat.depot.coeur; trefle = etat.depot.trefle; pique = etat.depot.pique; carreau = etat.depot.carreau +1}; ()
  | _ -> etat.depot <- {coeur = etat.depot.coeur; trefle = etat.depot.trefle + 1; pique = etat.depot.pique; carreau = etat.depot.carreau}; ()

(* vérifie que la carte a la bonne valeur pour entrer dans le dépot *)
let verif_valeur valeur dep = (valeur = (dep + 1)) 
  
(* met à jour le dépot en fonction du type de la carte *)
let maj_depot carte  = 
  match (Card.suit_to_string (snd carte)) with
  | "Pi" -> ajout_depot_val "Pi" ; ()
  | "Tr" -> ajout_depot_val "Tr" ; ()
  | "Co" -> ajout_depot_val "Co" ; ()
  | _ -> ajout_depot_val "Ca" ; () 
  
(* renvoie le bon dépot *)
let match_depot carte = 
  match (Card.suit_to_string (snd carte)) with 
  | "Pi" -> etat.depot.pique
  | "Tr" -> etat.depot.trefle
  | "Co" -> etat.depot.coeur
  | _ -> etat.depot.carreau

(* effectue un parcours du dépot tant qu'il y a des cartes qui puissent y être placé *)
let rec parcours_depot depot i boolean = 
  if (i = PArray.length etat.colonnes) then boolean 
  else let colonne = PArray.get etat.colonnes i in
  match colonne with
  | [] -> parcours_depot depot (i+1) boolean 
  | _ -> let derniere = ((List.length colonne) -1) in
    let carte = List.nth colonne derniere in
    let carte_string = Card.suit_to_string (snd carte) in
    if (verif_valeur (fst carte) (match_depot carte)) then 
      (set_colonnes (PArray.set etat.colonnes i (remove_last_card colonne)) ;
      ajout_depot_val carte_string ;
      parcours_depot depot (i+1) true)
    else (parcours_depot  depot (i+1) boolean)

(* parcours les registres pour voir si une carte est elligible pour être dans le dépot *)
let rec parcours_registre registre =
  match registre with
  | [] -> []
  | (a,b) :: l' -> if (verif_valeur a (match_depot (a,b))) then (maj_depot (a,b) ; parcours_registre l') else (a,b) :: parcours_registre l'

let verif_parcours_registre registre = 
  let taille_avant = List.length registre in
  let taille_apres = set_registre (parcours_registre registre); List.length etat.registre in 
  (not (taille_avant = taille_apres))

(* Normalisation du dépot -> effectué tant qu'une carte a reussi a être placé *)
let rec ajout_depot depot = if (parcours_depot etat.depot 0 false || (verif_parcours_registre etat.registre)) then (ajout_depot depot) else ()

(* Fonctions qui permettent de traiter les lignes d'un fichier *)

let traitement_ligne ligne = 
  match String.split_on_char ' ' ligne with
  | [string1;string2] -> traitement_composante1 (int_of_string string1) string2
  | _ -> raise Not_found (* ou false *)

(* on parcourt chaque ligne et on agit en conséquence*)
let rec parcourir lignes i = 
  (ajout_depot etat.depot);
  match lignes with
  | [] ->  if (verif_gagnant etat.depot) then (print_string "SUCCES\n" ; exit 0) else (affichage_depot etat.depot ; Printf.printf "ECHEC %d" i ; exit 1)
  | v :: l' -> if (not (traitement_ligne v)) then (affichage_depot etat.depot ; Printf.printf "ECHEC %d" i ; exit 1) else parcourir l' (i+1)
  
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None
    
let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line::acc)
    | None -> (List.rev acc)
  in
  aux []
    
let lines_of_file filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  close_in ic;
  (lines)

(* lis le fichier *)
let lire_fichier filename = 
  let line = lines_of_file filename in parcourir line 1
      
let check_mode filename =
  match filename with
  | Check filename -> lire_fichier filename
  | Search filename -> print_string "\n"

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  print_string "\nAffichage de la configuration initiale !";
  print_newline ();
  set_regles conf.game ;
  set_colonnes (construction_partie regles permut);
  set_registre (remplissage_registre conf.game permut) ;
  affichage_colonnes etat.colonnes;
  print_newline ();
  if (conf.game = Midnight || conf.game = Baker) then print_string "Pas de registre.\n" else (affichage_registre etat.registre);
  print_newline ();
  check_mode conf.mode ;
  exit 0

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()
