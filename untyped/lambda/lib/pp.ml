module F = Format

let pp_list ?(bracket = true) ~pp_elt fmt l =
  let rec loop fmt = function
    | [] ->
        ()
    | [x] ->
        pp_elt fmt x
    | x :: xs ->
        F.fprintf fmt "%a; %a" pp_elt x loop xs
  in
  if bracket then F.fprintf fmt "@[[%a]@]" loop l else F.fprintf fmt "@[%a@]" loop l
