import Text.Read

readEither' :: Read a => String -> Either String a 
readEither' xs = case readMaybe xs  of
                Nothing  -> Left $ "Cannot parse " ++ xs ++ " as Int"
                Just k -> Right k 

-- write a function that tries to parse all three Strings as Ints. 
-- If all the Strings can be successfully parsed as Ints, then we want to add those three Ints to get a sum. 
-- If one of the parses fails, we want to return a error message.

foo :: String -> String -> String -> Either String Int  
foo xs ys zs = case readEither' xs of 
            Left err -> Left err 
            Right k  -> case readEither' ys of
                        Left err -> Left err 
                        Right l  -> case readEither' zs of
                                    Left err -> Left err
                                    Right m  -> Right $ k+l+m  
        
-- To get rid of repetition like above, let us define a function bindEither using pattern matching
bindEither :: Either String  a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err 
bindEither (Right a) f = f a 

-- Using Infix notation
foo' :: String -> String -> String -> Either String Int
foo' xs ys zs = readEither' xs `bindEither` \k ->
                readEither' ys `bindEither` \l ->
                readEither' zs `bindEither` \m ->
                Right $ k + l+ m

--Using prefix notation
fooPrefix' :: String -> String -> String -> Either String Int
fooPrefix' xs ys zs = bindEither (readEither' xs :: Either String Int) (\k ->
                      bindEither (readEither' ys :: Either String Int) (\l ->
                      bindEither (readEither' zs :: Either String Int) (\m ->
                      Right $ k + l+ m )))           
