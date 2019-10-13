open Scheduler
open Netlist
open Netlist_ast

let nb_s = ref (-1)
let cr_s = ref 0
let env = ref Env.empty

let get_val = function
  | "1" | "t" -> Some true
  | "0" | "f" -> Some false
  | _ -> None

let str_val = function
  | VBit true -> "1"
  | VBit false -> "0"
  | VBitArray l ->
    Array.fold_left (fun s x -> if x then s^"1" else s^"0") "" l

let op x y = function
  | Or -> x || y
  | Xor -> (x && not y) || (not x && y)
  | And -> x && y
  | Nand -> not (x && y)

let arg_val = function
  | Avar x -> Env.find x !env
  | Aconst v -> v

let ch_val x v = match (Env.find x !env), v with
  | VBit _, VBit _ -> env := Env.add x v !env
  | (VBitArray lx), (VBitArray lv) ->
    if Array.length lx = Array.length lv then
      env := Env.add x v !env
    else
      raise (Invalid_argument "Arrays length not matching")
  | _ -> raise (Invalid_argument "Not the same type")

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
      | _ -> raise (Invalid_argument "Not the same type")
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
      | _ -> raise (Invalid_argument "Wrong types")
    end
  | x, Eslice(i1, i2, a) ->
    begin
      match arg_val a with
      | VBitArray l -> ch_val x (VBitArray (Array.sub l i1 (i2 - i1)))
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | x, Eselect(i, a) ->
    begin
      match arg_val a with
      | VBitArray l -> ch_val x (VBit l.(i))
      | _ -> raise (Invalid_argument "Wrong type")
    end
  | _ -> assert false

let read_inputs l =
  let rec get_bit x =
    print_string (x^": ");
    match get_val (read_line ()) with
    | Some b -> env := Env.add x (VBit b) !env
    | None -> print_string "Valeur incorrecte.\n"; get_bit x;
  and get_bitarray x l_x =
    print_string (x^": ");
    let s = read_line () in
    if String.length s <> Array.length l_x then
      begin
        print_string "Valeur incorrecte.\n";
        get_bitarray x l_x;
      end
    else
      begin
        String.iteri
          (fun i c -> match get_val (Char.escaped c) with
             | Some b -> l_x.(i) <- b
             | None ->
               print_string "Valeur incorrecte.\n";
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


let print_outputs l =
  List.iter
    (fun x -> print_string ("=> "^x^" = "^(str_val (Env.find x !env))^"\n"))
    l

let execute filename =
  let p = Scheduler.schedule (Netlist.read_file filename) in
  Env.iter
    (fun x t -> match t with
       | TBit -> env := (Env.add x (VBit false) !env);
       | TBitArray n ->
         env := Env.add x (VBitArray (Array.make n false)) !env)
    p.p_vars;
  while !cr_s <> !nb_s do
    incr cr_s;
    print_string ("Step "^(string_of_int !cr_s)^"\n");
    read_inputs p.p_inputs;
    List.iter eval_equation p.p_eqs;
    print_outputs p.p_outputs;
  done;
  exit 0

let main () =
  Arg.parse
    ["-n", Arg.Set_int nb_s, "Number of steps in the simulation"]
    execute
    ""
;;

main ()
