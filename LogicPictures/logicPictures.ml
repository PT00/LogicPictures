let ( let* ) xs ys = List.concat_map ys xs

let rec choose m n =
  if m > n then [] else m :: choose (m+1) n

let build_row ps n = 
  let rec fill_true n = 
    if n <= 0 then [] 
    else true :: fill_true (n - 1) in 

  let rec fill_false n = 
    if n <= 0 then [] 
    else false :: fill_false (n - 1) in 

  let rec build acc left ps =
    match ps, left with
    | [], _ -> [acc @ fill_false left] 
    | x::xs, l ->
      let* space = choose 0 
      (l - x - (if xs = [] then 0 else 1) - List.length xs) in 
      let field = fill_true x in
      let blank = fill_false space in
      let new_acc = acc @ blank @ field in
      let new_left = left - x - space - (if xs = [] then 0 else 1) in 
      if xs = [] then
        [new_acc @ fill_false new_left]
      else
        build (new_acc @ [false]) new_left xs
  in build [] n ps;;

let build_candidate pss n = 
  let rec build pss acc =
    match pss with
    | [] -> [List.rev acc]
    | spec::rest_pss -> 
      let* row = build_row spec n in 
      build rest_pss (row :: acc) 
  in build pss [];;

let verify_row ps xs = 
  let rec skip_false xs = 
    match xs with 
    | false :: rest -> skip_false rest
    | _ -> xs in
  let rec count_true xs acc = 
    match xs with 
    | true :: rest -> count_true rest (acc + 1)
    | _ -> (acc, xs) in

  let rec verify ps xs =
    match ps, xs with
    | [], [] -> true 
    | _, [] -> ps = [0] || ps = [] 

    | [], _ -> skip_false xs = []  
    | s :: spec_rest, r ->
      let r = skip_false r in 
      let true_count, rest_xs = count_true r 0 in 
      if s = true_count then
        verify spec_rest (skip_false rest_xs)  
      else
        false in verify ps (skip_false xs);; 

let verify_rows pss xss = 
  let rec verify pss xss = 
    match pss, xss with
    | [], [] -> true
    | spec::pss_tail, row::xss_tail ->
      if verify_row spec row then 
        verify pss_tail xss_tail else false 
    | _ -> false  
  in
  verify pss xss;;

let transpose xss = 
      let rec empty xs = 
        match xs with 
      | [] -> true
      | [] :: t -> empty t
      | _ :: _ -> false in
    let rec heads xs = 
      match xs with
      | [] -> []
      | [] :: t -> heads t
      | (h :: _) :: t -> h :: heads t in
    let rec tails xs = 
      match xs with 
      | [] -> []
      | [] :: t -> tails t
      | (_ :: t) :: tail -> t :: tails tail in

    let rec transpose_rec xss =
      if empty xss then [] 
      else heads xss :: transpose_rec (tails xss) in transpose_rec xss;; (* Transpozycja listy to po prostu stworzenie list z elementami na tym samym i-tym indeksie, zatem bierzemy zawsze glowy*)

type nonogram_spec = {rows: int list list; cols: int list list}

let solve_nonogram nono =
  build_candidate (nono.rows) (List.length (nono.cols))
  |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols)

let example_1 = {
  rows = [[2];[1];[1]];
  cols = [[1;1];[2]]
}

let example_2 = {
  rows = [[2];[2;1];[1;1];[2]];
  cols = [[2];[2;1];[1;1];[2]]
}

let big_example = {
  rows = [[1;2];[2];[1];[1];[2];[2;4];[2;6];[8];[1;1];[2;2]];
  cols = [[2];[3];[1];[2;1];[5];[4];[1;4;1];[1;5];[2;2];[2;1]]
}