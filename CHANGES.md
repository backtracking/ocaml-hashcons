
# 1.4.0
  - fixed performance bugs in weak hash tables implementation,
    by back porting some old fixes from OCaml's `Weak` module
    (reported by Edwin Török)
  - improved equality functions in Hset and Hmap
    (contributed by Dorian Lesbre)
  - a lot of missing functions in Hset and Hmap wrt OCaml's Set and Map,
    with the notable exception of `to_seq_rev`
    (contributed by Dorian Lesbre)

# 1.3
  - modules Hset and Hmap moved into module Hashcons, to avoid the clash with
    a module Hmap from another project (patch from Qi LI)

# 1.2
  - fixed bug in Hset (reported by Jan Midtgaard)

# 1.1
 - do not assume anymore that the hash function returns a nonnegative value

# 1.0, 09/09/2013
 - code moved to github
