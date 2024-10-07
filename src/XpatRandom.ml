(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (between 1 and 999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).

*)

(* The numbers manipulated below will be in [0..randmax[ *)
let randmax = 1_000_000_000

(* Converting an integer n in [0..randmax[ to an integer in [0..limit[ *)
let reduce n limit =
  Int.(of_float (to_float n /. to_float randmax *. to_float limit))

(* renvoie la valeur de la composante2 en fonctions des deux dernières valeurs *)
let difference a b = 
    match (b <= a) with
    | true -> (a-b)
    | false -> (a-b+randmax)

(* renvoie l'élement à placer dans la composante2 de la paire n *)
let secondeComposante l1 l2 n = 
    match l2 with
    | b :: a :: _ -> difference a b
    | _ -> failwith "erreur"

(* renvoie les 55 paires créées *)
let creer55Paires l1 l2 n = 
    let rec parcoursPaire l1 l2 n =
        match n with
        | 55 -> List.combine l1 l2
        | _ -> let un = (((List.hd l1) + 21) mod 55) in let deux = (secondeComposante l1 l2 n) in parcoursPaire (un :: l1) (deux :: l2) (n+1)
    in parcoursPaire (21 :: 0 :: l1) (1 :: n :: l2) 2

(* renvoie les 2 FIFO créées *)
let rec placerFifo tab n l2 = 
    match n with
    | 24 -> (Fifo.of_list l2,Fifo.of_list (List.rev tab))
    | _ -> match l2 with
        | v :: l' -> placerFifo (v :: tab) (n+1) l'
        | _ -> failwith "erreur"

(* renvoie le tirage du 1er element de chaque FIFO, ainsi que leur reste *)
let tirage f1 f2 n = 
    let (a,f1'), (b,f2') = Fifo.pop f1, Fifo.pop f2 in 
    let p = difference a b in
    reduce p n, Fifo.push b f1', Fifo.push p f2' 

(* pareil mais 165 fois *)
let rec tirage165 f1 f2 n = 
    match n with 
    | 165 -> (f1,f2)
    | _ -> let d,f1',f2' = tirage f1 f2 n in tirage165 f1' f2' (n+1)

(* renvoie la permutation *)
let rec permutation f1 f2 tab list =
    let n = List.length list in 
    match n with
    | 0 -> tab
    | _ -> let p,f1,f2 = tirage f1 f2 n in let nbr = List.nth list p in permutation f1 f2 (nbr :: tab) (List.filter (fun x -> x != nbr) list)

let shuffle n =
    let listePaireTriee = List.sort (fun (x1,_) (x2,_)  -> compare x1 x2) (creer55Paires [] [] n) in
    let (_ ,l2) = List.split listePaireTriee
    in let f1_init, f2_init = placerFifo [] 0 l2
    in let f1, f2 = tirage165 f1_init f2_init 0 
    in permutation f1 f2 [] (0 :: List.init 51 (fun x -> x + 1))
    
