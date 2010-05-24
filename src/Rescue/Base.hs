module Rescue.Base where

    import Text.Regex(mkRegex, subRegex)
    
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

    replace :: String -> String -> String -> String
    replace rep with str = subRegex (mkRegex rep) str with