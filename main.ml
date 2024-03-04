

(*main関数*)
let main argv = 
  (*引数の個数チェック*)
  if (Array.length Sys.argv) < 2 then
    print_endline "引数の個数が正しくありません"
  else
    ();;
  
    print_endline ".intel_syntax noprefix";
    print_endline ".globl main";
    print_endline "main:";
    print_endline ("  mov rax, " ^ string_of_int (int_of_string Sys.argv.(1)));
    print_endline "  ret";;

let () = main()