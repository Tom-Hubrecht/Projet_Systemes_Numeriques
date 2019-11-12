open Netlist_ast
open Format

exception Illegal_character of string
exception Value_error of string

type memory = {
  mutable env : value Env.t;
  rom : (int * int * (int, value) Hashtbl.t) Env.t;
  ram : (int * int * (int, value) Hashtbl.t) Env.t
}

let int_of_bool = function
  | true -> 1
  | false -> 0

let bool_of_string = function
  | "1" | "t" -> Some true
  | "0" | "f" -> Some false
  | _ -> None

let bool_of_char c = bool_of_string (Char.escaped c)

let rec pow x = function
  | 0 -> 1
  | 1 -> x
  | n -> let r = (pow x (n/2)) in r * r * (pow x (n mod 2))

let string_of_bit = function
  | VBit true -> "1"
  | VBit false -> "0"
  | VBitArray l -> Array.fold_left (fun s x -> if x then s^"1" else s^"0") "" l

let string_of_arg = function
  | Avar x -> x
  | Aconst v -> string_of_bit v

let bit_of_string s = function
  | TBit ->
    begin
      match bool_of_string s with
      | Some b -> Some (VBit b)
      | None -> None
    end
  | TBitArray n ->
    if String.length s <> n then
      None
    else
      begin
        let l = Array.make n false and b = ref true in
        String.iteri
          (fun i c -> match bool_of_char c with
             | Some b -> l.(i) <- b
             | None -> b := false)
          s;
        if !b then
          Some (VBitArray l)
        else
          None
      end

let ask_bit x = function
  | TBit -> printf "%s: @?" x; bit_of_string (read_line ()) TBit
  | TBitArray n as t_x ->
    printf "%s[%d]: @?" x n;
    bit_of_string (read_line ()) t_x

let int_of_bit = function
  | VBit b -> int_of_bool b
  | VBitArray v -> Array.fold_left (fun k b -> 2 * k + (int_of_bool b)) 0 v

let op x y = function
  | Or -> x || y
  | Xor -> (x && not y) || (not x && y)
  | And -> x && y
  | Nand -> not (x && y)

let bit_of_arg env = function
  | Avar x -> Env.find x env
  | Aconst v -> v

let ch_val x v mem = match (Env.find x mem.env), v with
  | VBit _, VBit _ -> mem.env <- Env.add x v mem.env
  | (VBitArray lx), (VBitArray lv) ->
    if Array.length lx = Array.length lv then
      mem.env <- Env.add x v mem.env
    else
      raise (Value_error "Arrays length not matching")
  | VBit _, (VBitArray [|b|]) -> mem.env <- Env.add x (VBit b) mem.env
  | (VBitArray [|_|]), VBit b -> mem.env <- Env.add x (VBitArray [|b|]) mem.env
  | VBit _, VBitArray l | VBitArray l, VBit _ ->
    raise (Value_error (sprintf "Incompatible types : VBit and VBitArray[%d]"
                          (Array.length l)))

let from_rom x a mem =
  let (a_s, w_s, x_rom) = Env.find x mem.rom
  and i_a = int_of_bit (bit_of_arg mem.env a)
  in
  match Hashtbl.find_opt x_rom i_a with
  | Some v -> v
  | None -> let v = VBitArray (Array.make w_s false) in
    Hashtbl.add x_rom i_a v;
    v

let from_ram x a mem =
  let (a_s, w_s, x_ram) = Env.find x mem.ram
  and i_a = int_of_bit (bit_of_arg mem.env a)
  in
  match Hashtbl.find_opt x_ram i_a with
  | Some v -> v
  | None -> let v = VBitArray (Array.make w_s false) in
    Hashtbl.add x_ram i_a v;
    v

let ch_ram x w_a w_d mem =
  let (_, _, x_ram) = Env.find x mem.ram in
  Hashtbl.replace x_ram (int_of_bit (bit_of_arg mem.env w_a))
    (bit_of_arg mem.env w_d)
