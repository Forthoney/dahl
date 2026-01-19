structure Scope =
struct
  type local_var = { name : string, depth : int, reg : int }
  type t =
    { locals : local_var list
    , currentDepth : int
    }

  val new = { locals = [], currentDepth = 0 }

  fun begin {locals, currentDepth} =
    {locals = locals, currentDepth = currentDepth + 1}

  fun exit {locals, currentDepth} =
    let
      val newDepth = currentDepth - 1
      fun loop [] = {locals = [], currentDepth = newDepth}
        | loop (locals as {name, depth}::ls) =
          if depth > newDepth then
            loop ls
          else
            {locals = locals, currentDepth = newDepth}
    in
      loop locals
    end

  fun add {locals, currentDepth} (name, reg) =
    { locals = {name = name, depth = currentDepth, reg = reg}::locals
    , currentDepth = currentDepth
    }

  fun resolve {locals, currentDepth} name =
    let
      fun loop ({name = name', depth, reg}::locals) =
          if name = name' then SOME reg
          else loop locals
        | loop [] = NONE
    in
      loop locals
    end
end
