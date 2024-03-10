type task = {
  description: string;
  mutable done: bool;
}

let tasks = ref [];;

let add_task desc =
  tasks := {description = desc; done = false} :: !tasks;;

let mark_task_done n =
  let rec helper lst i = match lst with
    | [] -> ()
    | h::t -> if i = n then h.done <- true else helper t (i+1)
  in helper !tasks 0;;

let list_tasks () =
  let rec aux i = function
    | [] -> ()
    | h::t ->
      Printf.printf "%d: %s [%s]\n" i h.description (if h.done then "Done" else "Not Done");
      aux (i+1) t
  in aux 1 !tasks;;

let rec main_loop () =
  Printf.printf "\nCommands: add <desc>, done <n>, list, quit\n%!";
  match read_line () with
  | "quit" -> Printf.printf "Exiting...\n%!"
  | s ->
      (try
         match String.split_on_char ' ' s with
         | ["add"; desc] -> add_task desc
         | ["done"; n] -> mark_task_done (int_of_string n - 1)
         | ["list"] -> list_tasks ()
         | _ -> raise (Failure "unknown command")
       with
       | Failure err -> Printf.printf "Error: %s\n%!" err
       | _ -> Printf.printf "Invalid command or number.\n%!");
      main_loop ();;

let () =
  Printf.printf "OCaml Todo List Application\n%!";
  main_loop ();;
