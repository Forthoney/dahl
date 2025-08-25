Double-quoted
  $ echo '"Hello, world!"' | dahl
  STRING Hello, world!

Single-quoted
  $ echo "'Hello, world!'" | dahl
  STRING Hello, world!

Escaped double quote
  $ echo '"\"Hello, world!\""' | dahl
  STRING "Hello, world!"

Escaped single quote
  $ echo "'\\'Hello, world!\\''" | dahl
  STRING 'Hello, world!'
