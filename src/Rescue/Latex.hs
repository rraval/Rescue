module Rescue.Latex where

    data Resume a = Resume (String -> String)

    instance Monad Resume where
        (Resume r) >> f = Resume $ r . a
            where (Resume a) = f

        (>>=)  = undefined
        return = undefined

    instance Show (Resume a) where
        show (Resume a) = a ""


    surround :: String -> String -> Resume a -> Resume a
    surround start end resume = do
      Resume (start++)
      resume
      Resume (end++)
                      
    section :: String -> Resume a -> Resume a
    section s = surround ("\\begin{ressection}{" ++ s ++ "}\n")
                         "\\end{ressection}"

    item :: String -> Resume a
    item s = Resume $ \n -> "\\resitem{" ++ s ++ "}\n" ++ n