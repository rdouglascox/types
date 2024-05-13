{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples where

allexamples = [commutativityofproducts, commutativityofsums]

-- (A*B) -> (B*A)
commutativityofproducts = "\\x:(A*B).{snd x,fst x}"

-- (A+B) -> (B+A)
commutativityofsums = "\\x1:(A+B).(case x1 of (\\x2:A.(inr x2 as (B+A))) | (\\x3:B.(inl x3 as (B+A))))"

-- ((A*B)*C) -> (A*(B*C))
associativityofproducts1 = undefined

-- (A(B*C)) -> ((A*B)*C)
associativityofproducts2 = undefined

-- ((A+B)+C) -> (A+(B+C))
associativityofsums1 = undefined

-- (A+(B+C)) -> ((A+B)+C)
associativityofsums2 = undefined

-- prove (A -> (B -> A))
test3 = "\\x:A.(\\y:B.x)"

-- prove ((A -> (B -> C)) -> ((A->B) -> (A->C)))
test4 =
  "\\x:(A->(B->C)).\\y:(A->B).(\\x2:A.((\\x3:A.(\\z1:A.((y z1)) (x1 x2) )))(x2 y))"

-- prove ((A->(B->C))->((A+B)->C))
test6 =
  "\\x1:(A->(B->C)).\\x4:(A*B).(((x1 fst x4) snd x4))"

-- prove ((A->(B->C))->((B+A)->C))
test7 =
  "\\x1:(A->(B->C)).\\x4:(B*A).(((x1 snd x4) fst x4))"

-- prove ((A->(B->C))->((A+B)->C))
test8 =
  "\\x1:(A->(B->C)).\\x4:(A*B).(((x1 fst x4) snd x4))"

-- prove (A -> ((A -> Bot) -> Bot))
test9 =
  "\\x:A.\\y:(A->Bot).(y x)"

-- prove ({A,{B,C}} -> {{A,B},C})
test10 = "\\x1:(A*(B*C)).{{fst x1,fst (snd x1)},snd (snd x1)}"
