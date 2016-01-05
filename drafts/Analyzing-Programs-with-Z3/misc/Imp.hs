type Name = String

data AExp = Lit Int
          | Var Name
          | AExp :+: AExp
          | AExp :-: AExp
          | AExp :*: AExp

data BExp = True'
          | False'
          | AExp :<=: AExp
          | AExp :==: AExp
          | BExp :|: BExp
          | BExp :&: BExp
          | Not BExp

data Cmd = Skip
         | Set Name AExp
         | Seq Cmd Cmd
         | If BExp Cmd Cmd
         | While BExp Cmd

