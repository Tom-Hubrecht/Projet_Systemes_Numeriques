open Netlist
open Netlist_ast
open Netlist_printer

let nb_s = ref (-1)
let cr_s = ref 0
let pr_p = ref false
let rom_file = ref ""
let env = ref Env.empty
let rom = ref Env.empty
let ram = ref Env.empty
let w_ram = ref []

let int_of_bool = function
  | true -> 1
  | false -> 0

let rec pow x = function
  | 0 -> 1
  | 1 -> x
  | n -> let r = (pow x (n/2)) in r * r * (pow x (n mod 2))

let int_val = function
  | VBitArray v ->
    let k = ref 0 in
    Array.iteri (fun i b -> k := 2 * !k + (int_of_bool b)) v;
    !k
  | VBit b -> if b then 1 else 0

let get_val = function
  | "1" | "t" -> Some true
  | "0" | "f" -> Some false
  | _ -> None

let str_val = function
  | VBit true -> "1"
  | VBit false -> "0"
  | VBitArray l ->
    Array.fold_left (fun s x -> if x then s^"1" else s^"0") "" l

let str_arg = function
  | Avar x -> x
  | Aconst v -> str_val v

let op x y = function
  | Or -> x || y
  | Xor -> (x && not y) || (not x && y)
  | And -> x && y
  | Nand -> not (x && y)

let arg_val = function
  | Avar x -> Env.find x !env
  | Aconst v -> v

let fill_rom () =
  try
    let r = open_in !rom_file in
    let v = ref true and x = ref "" in
    while true do
      let s = input_line r in
      if !v then
        x := s
      else
        begin
          let (m, k), x_rom = Env.find !x !rom in
          String.iteri
            (fun i c ->
               if i < m*k then
                 begin
                   match get_val (Char.escaped c) with
                   | Some b ->
                     let VBitArray l = x_rom.(i / k) in
                     l.(i mod k) <- b;
                     x_rom.(i / k) <- VBitArray l;
                   | None -> ()
                 end)
            s;
        end
    done;
  with
  | _ -> raise Not_found

let ch_val x v = match (Env.find x !env), v with
  | VBit _, VBit _ -> env := Env.add x v !env
  | (VBitArray lx), (VBitArray lv) ->
    if Array.length lx = Array.length lv then
      env := Env.add x v !env
    else
      raise (Invalid_argument "Arrays length not matching")
  | VBit _, (VBitArray [|b|]) -> env := Env.add x (VBit b) !env
  | (VBitArray [|_|]), VBit b -> env := Env.add x (VBitArray [|b|]) !env
  | _ -> raise (Invalid_argument "Not the same type(ch_val)")

let eval_equation = function
  | x, (Earg a) -> ch_val x (arg_val a)
  | x, (Ereg r) -> ch_val x (Env.find r !env)
  | x, (Enot a) ->
    begin
      match arg_val a with
      | VBit b -> ch_val x (VBit (not b))
      | VBitArray l -> ch_val x (VBitArray (Array.map (fun b -> not b) l))
    end
  | x, Ebinop(o, a1, a2) ->
    begin
      match (arg_val a1), (arg_val a2) with
      | (VBit b1), (VBit b2) -> ch_val x (VBit (op b1 b2 o))
      | (VBitArray l1), (VBitArray l2) ->
        ch_val x (VBitArray (Array.map2 (fun x y -> op x y o) l1 l2))
      | _ -> raise (Invalid_argument "Not the same type(binop)")
    end
  | x, Emux(m, a1, a2) ->
    begin
      match arg_val m with
      | VBit b ->
        if b then ch_val x (arg_val a1) else ch_val x (arg_val a2)
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | x, Econcat(a1, a2) ->
    begin
      match (arg_val a1), (arg_val a2) with
      | (VBitArray l1), (VBitArray l2) ->
        ch_val x (VBitArray (Array.append l1 l2))
      | (VBit b1), (VBit b2) -> ch_val x (VBitArray [|b1; b2|])
      | (VBit b), (VBitArray l) ->
        ch_val x (VBitArray (Array.append [|b|] l))
      | (VBitArray l), (VBit b) ->
        ch_val x (VBitArray (Array.append l [|b|]))
    end
  | x, Eslice(i1, i2, a) ->
    begin
      match arg_val a with
      | VBitArray l -> ch_val x (VBitArray (Array.sub l i1 (i2 - i1 + 1)))
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | x, Eselect(i, a) ->
    begin
      match arg_val a with
      | VBitArray l -> ch_val x (VBit l.(i))
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | x, Erom(_, _, a) -> ch_val x (snd (Env.find x !rom)).(int_val (arg_val a));
  | x, Eram(_, _, r_a, w_e, w_a, w_d) ->
    ch_val x (Env.find x !ram).(int_val (arg_val r_a))

let read_inputs l =
  let rec get_bit x =
    Format.printf "@[%s: @]@?" x;
    let c = read_line () in
    match get_val c with
    | Some b -> env := Env.add x (VBit b) !env
    | None ->
      begin
        if String.length c = 1 then
          Format.printf "@[Caractère illégal : %s.@]@." c
        else
          Format.printf "@[Valeur incorrecte, un bit est attendue mais un array est fourni.@]@.";
        get_bit x;
      end
  and get_bitarray x l_x =
    Format.printf "@[%s[%d]: @]@?" x (Array.length l_x);
    let s = read_line () in
    if String.length s <> Array.length l_x then
      begin
        Format.printf "@[Valeur incorrecte, un array de taille %d est attendu mais l'array fourni est de taille %d@].@."
          (Array.length l_x) (String.length s);
        get_bitarray x l_x;
      end
    else
      begin
        String.iteri
          (fun i c -> match get_val (Char.escaped c) with
             | Some b -> l_x.(i) <- b
             | None ->
               Format.printf "@[Caractère illégal : %c.@]@." c;
               get_bitarray x l_x;)
          s;
        env := Env.add x (VBitArray l_x) !env;
      end
  in
  List.iter
    (fun x -> match Env.find x !env with
       | VBit _ -> get_bit x;
       | VBitArray l_x -> get_bitarray x l_x;)
    l

let eval_ram (x, w_e, w_a, w_d) = match arg_val w_e with
  | VBit true ->
    let x_ram = Env.find x !ram in
    x_ram.(int_val (arg_val w_a)) <- arg_val w_d;
  | VBit false -> ()
  | _ -> raise (Invalid_argument ("Une valeur VBit est attendue mais "
                                  ^(str_arg w_d)^" est un VBitArray"))

let print_outputs l =
  List.iter
    (fun x -> Format.printf "@[=> %s = %s@]@." x (str_val (Env.find x !env)))
    l

let execute filename =
  let p = Netlist.read_file filename in
  begin
    try
      let s_p = Scheduler.schedule p in
      if !pr_p then Netlist_printer.print_program stdout s_p;
      Env.iter
        (fun x t -> match t with
           | TBit -> env := (Env.add x (VBit false) !env);
           | TBitArray n ->
             env := Env.add x (VBitArray (Array.make n false)) !env)
        s_p.p_vars;
      List.iter
        (fun (x, e) -> match e with
           | Erom(a_s, w_s, _) ->
             rom := Env.add x (((pow 2 a_s), w_s),
                               (Array.make (pow 2 a_s) (VBitArray (Array.make w_s false))))
                 !rom;
           | Eram(a_s, w_s, _, w_e, w_a, w_d) ->
             ram := Env.add x
                 (Array.make (pow 2 a_s) (VBitArray (Array.make w_s false)))
                 !ram;
             w_ram := (x, w_e, w_a, w_d)::!w_ram;
           | _ -> ())
        s_p.p_eqs;
      if !rom_file <> "" then fill_rom ();
      while !cr_s <> !nb_s do
        incr cr_s;
        Format.printf "@[Step %d@]@." !cr_s;
        read_inputs s_p.p_inputs;
        List.iter eval_equation s_p.p_eqs;
        List.iter eval_ram !w_ram;
        print_outputs s_p.p_outputs;
      done;
    with
    | Scheduler.Combinational_cycle ->
      Format.eprintf "The netlist has a combinatory cycle.@.";
      exit 2;
  end;
  exit 0

let main () =
  Arg.parse
    ["-n", Arg.Set_int nb_s, "Number of steps in the simulation"
    ;"-p", Arg.Set pr_p, "Prints the program before execution"
    ;"-rom", Arg.Set_string rom_file, "File to search for the roms"
    ]
    execute
    ""
;;

main ()
