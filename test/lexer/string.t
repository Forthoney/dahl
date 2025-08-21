Double-quoted
  $ echo '"Hello, world!"' | dahl
  Str Hello, world!

Single-quoted
  $ echo "'Hello, world!'" | dahl
  Str Hello, world!

Escaped double quote
  $ echo '"\"Hello, world!\""' | dahl
  Str "Hello, world!"

Escaped single quote
  $ echo "'\\'Hello, world!\\''" | dahl
  Str 'Hello, world!'
