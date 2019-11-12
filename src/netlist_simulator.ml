open Netlist
open Netlist_ast
open Netlist_aux

let nb_s = ref (-1)
let cr_s = ref 0
let pr_p = ref false
let rom_file = ref ""

let fill_rom rom =
  let r = open_in !rom_file in
  let v = ref true and x = ref "" in
  try
    while true do
      let s = input_line r in
      if !v then
        x := s
      else
        begin
          let m, k, x_rom = Env.find !x rom in
          let i = ref 0 and j = ref 0 in
          let v = Array.make k false in
          let l = ref v in
          while (k * !i + !j) < String.length s && !i < (pow 2 m) do
            if !j = 0 then l := v;
            begin
              match bool_of_char s.[k * !i + !j] with
              | Some b -> !l.(!j) <- b
              | None -> ()
            end;
            if !j = k - 1 then
              begin
                Hashtbl.replace x_rom !i (VBitArray (Array.copy !l));
                j := 0;
                i := !i + 1;
              end
            else
              j := !j + 1;
          done;
        end;
      v := not !v;
    done;
  with End_of_file -> close_in r

let eval_equation mem = function
  | x, (Earg a) -> ch_val x (bit_of_arg mem.env a) mem
  | x, (Ereg r) -> () (*ch_val x (Env.find r mem.env) mem*)
  | x, (Enot a) ->
    begin
      match bit_of_arg mem.env a with
      | VBit b -> ch_val x (VBit (not b)) mem
      | VBitArray l -> ch_val x (VBitArray (Array.map (fun b -> not b) l)) mem
    end
  | x, Ebinop(o, a1, a2) ->
    begin
      match (bit_of_arg mem.env a1), (bit_of_arg mem.env a2) with
      | (VBit b1), (VBit b2) -> ch_val x (VBit (op b1 b2 o)) mem
      | (VBitArray l1), (VBitArray l2) ->
        ch_val x (VBitArray (Array.map2 (fun x y -> op x y o) l1 l2)) mem
      | _ -> raise (Invalid_argument "Not the same type(binop)")
    end
  | x, Emux(m, a1, a2) ->
    begin
      match bit_of_arg mem.env m with
      | VBit b ->
        if b then
          ch_val x (bit_of_arg mem.env a1) mem
        else
          ch_val x (bit_of_arg mem.env a2) mem
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | x, Econcat(a1, a2) ->
    begin
      match (bit_of_arg mem.env a1), (bit_of_arg mem.env a2) with
      | (VBitArray l1), (VBitArray l2) ->
        ch_val x (VBitArray (Array.append l1 l2)) mem
      | (VBit b1), (VBit b2) -> ch_val x (VBitArray [|b1; b2|]) mem
      | (VBit b), (VBitArray l) ->
        ch_val x (VBitArray (Array.append [|b|] l)) mem
      | (VBitArray l), (VBit b) ->
        ch_val x (VBitArray (Array.append l [|b|])) mem
    end
  | x, Eslice(i1, i2, a) ->
    begin
      match bit_of_arg mem.env a with
      | VBitArray l -> ch_val x (VBitArray (Array.sub l i1 (i2 - i1 + 1))) mem
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | x, Eselect(i, a) ->
    begin
      match bit_of_arg mem.env a with
      | VBitArray l -> ch_val x (VBit l.(i)) mem
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | x, Erom(_, _, a) ->
    ch_val x (from_rom x a mem) mem
  | x, Eram(_, _, a, _, _, _) ->
    ch_val x (from_ram x a mem) mem

let read_inputs l env =
  let rec inp_aux e x =
    let t_x =
      (match Env.find x e with
       | VBit _ -> TBit
       | VBitArray l -> TBitArray (Array.length l))
    in
    match ask_bit x t_x with
    | Some v -> Env.add x v e
    | None ->
      Format.printf "Valeur incorrecte pour %s.@." x;
      inp_aux e x
  in
  List.fold_left inp_aux env l

let eval_ram mem (x, w_e, w_a, w_d) = match bit_of_arg mem.env w_e with
  | VBit true | VBitArray [|true|] -> ch_ram x w_a w_d mem
  | VBit false | VBitArray [|false|] -> ()
  | _ -> raise (Invalid_argument ("Une valeur VBit est attendue mais "
                                  ^(string_of_arg w_d)^" est un VBitArray"))

let eval_reg mem (x, r) = ch_val x (Env.find r mem.env) mem

let print_outputs l env =
  List.iter
    (fun x ->
       Format.printf "=> %s = %s@." x (string_of_bit (Env.find x env)))
    l

let execute filename =
  let p = Netlist.read_file filename in
  begin
    try
      let s_p = Scheduler.schedule p in
      if !pr_p then Netlist_printer.print_program stdout s_p;
      let env = Env.fold
          (fun x t e -> match t with
             | TBit -> Env.add x (VBit false) e
             | TBitArray n -> Env.add x (VBitArray (Array.make n false)) e)
          s_p.p_vars Env.empty in
      let l_reg, w_ram, rom, ram = List.fold_left
          (fun (r, l, ro, ra) (x, e) -> match e with
             | Erom(a_s, w_s, _) ->
               (r, l, (Env.add x (a_s, w_s, Hashtbl.create a_s) ro), ra)
             | Eram(a_s, w_s, _, w_e, w_a, w_d) ->
               (r, (x, w_e, w_a, w_d)::l, ro,
                (Env.add x (a_s, w_s, Hashtbl.create a_s) ra))
             | Ereg a -> ((x, a)::r, l, ro, ra)
             | _ -> (r, l, ro, ra))
          ([], [], Env.empty, Env.empty) s_p.p_eqs in
      let mem = {env = env; rom = rom; ram = ram} in
      if !rom_file <> "" then
        fill_rom mem.rom;
      while !cr_s <> !nb_s do
        incr cr_s;
        Format.printf "Step %d@." !cr_s;
        mem.env <- read_inputs s_p.p_inputs mem.env;
        List.iter (eval_equation mem) s_p.p_eqs;
        List.iter (eval_ram mem) w_ram;
        print_outputs s_p.p_outputs mem.env;
        List.iter (eval_reg mem) l_reg;
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
