type tokenKind = 
TK_RESERVED of int
| TK_NUM of int
| TK_EOF

type token = {
  mutable kind : tokenKind;
  mutable str : string;
  mutable value : int;
  mutable str_ : string
}
(*// エラーを報告するための関数
// printfと同じ引数を取る*)
let error fmt = 
  ""
(*// 次のトークンが期待している記号のときには、トークンを1つ読み進めて
// 真を返す。それ以外の場合には偽を返す。*)
let comsume op = 
  ()

  (*// 次のトークンが期待している記号のときには、トークンを1つ読み進める。
// それ以外の場合にはエラーを報告する。*)
  let except op = 
    ()

  (*// 次のトークンが数値の場合、トークンを1つ読み進めてその数値を返す。
// それ以外の場合にはエラーを報告する。*)
let except_number = ()

let at_eof =
  ()

  (*// 新しいトークンを作成してcurに繋げる*)
let new_token kind cur str_ = 
  ()

  (*// 入力文字列pをトークナイズしてそれを返す*)
let tokenize p = 
  ()

(*main関数*)
let main argv = 
  (*引数の個数チェック*)
  if (Array.length Sys.argv) < 2 then
    print_endline "引数の個数が正しくありません"
  else
    ();;
    let p = Sys.argv.(1);;

    let rec strtol pointer num_pointer s lst=
      if pointer < (String.length s) then
          if (s.[pointer] = '+') then
            let number =  (String.sub s num_pointer (pointer - num_pointer)) in
            (strtol (pointer + 1) (pointer + 1) s) ((lst @ [number]) @ ["add"])
            
          else if s.[pointer] = '-' then
            let number =  (String.sub s num_pointer (pointer - num_pointer)) in
            (strtol (pointer + 1) (pointer + 1) s) (lst @ [number] @ ["sub"])
          else if s.[pointer] = ' ' then
                if num_pointer = pointer then
                  strtol (pointer + 1) (num_pointer + 1) s lst
                else strtol (pointer + 1) (num_pointer) s lst

          else strtol (pointer + 1) num_pointer s  lst
      else
        let number = (String.sub s num_pointer (pointer - num_pointer)) in
            (lst @ [number]);;
    
    print_endline ".intel_syntax noprefix";
    print_endline ".globl main";
    print_endline "main:";
    
    let lst = (strtol 0 0 p []) in 
    print_endline ("  mov rax, " ^ (List.hd lst));
    let rec print_lst l = match l with
      | [] -> print_endline "  ret"
      | first :: rest ->
        if first = "add" then 
          print_string "add rax, rdi, "
        else if first = "sub" then
          print_string "sub rax, rdi,"
        else print_endline first;
      print_lst rest
      in print_lst (List.tl lst);;

let () = main()