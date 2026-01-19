structure Scope =
struct
  type local_var = { name : string, depth : int }
  type t =
    { locals : local_var list
    , currentDepth : int
    }

  val new = { locals = [], currentDepth = 0 }

  fun begin {locals, currentDepth} =
    {locals = locals, currentDepth = currentDepth + 1}

  fun exit {locals, currentDepth} =
    {locals = locals, currentDepth = currentDepth - 1}

  fun add {locals, currentDepth} name =
    { locals = {name = name, depth = currentDepth}::locals
    , currentDepth = currentDepth
    }

  fun resolve {locals, currentDepth} name =
    let
      fun loop i ({name = name', depth}::locals) =
          if name = name' then SOME i
          else loop (i + 1) locals
        | loop i [] = NONE
    in
      loop 0 locals
    end
end
