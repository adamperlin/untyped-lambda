 data Term = X | Y
 newtype Env = Env ([(String, Closure)])
 type Closure = (Term, Env)

 main = return ()
