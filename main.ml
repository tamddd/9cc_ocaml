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