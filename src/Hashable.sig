signature HASHABLE =
sig
  include EQ
  val hash : t -> Word64.word
end
