
import Text.Read

-- write a function that tries to parse all three Strings as Ints. 
-- If all the Strings can be successfully parsed as Ints, then we want to add those three Ints to get a sum. 
-- If one of the parses fails, we want to return Nothing.

foo :: String -> String -> String -> Maybe Int 
foo xs ys zs = case readMaybe xs of 
            Nothing -> Nothing 
            Just k -> case readMaybe ys of
                        Nothing -> Nothing 
                        Just l -> case readMaybe zs of
                                    Nothing  -> Nothing
                                    Just m -> Just $ k+l+m  


-- To get rid of repetition like above, let us define a function bindMaybe using pattern matching
bindMaybe :: Maybe  a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a 

-- Using Infix notation
foo' :: String -> String -> String -> Maybe Int
foo' xs ys zs = readMaybe xs `bindMaybe` \k ->
                readMaybe ys `bindMaybe` \l ->
                readMaybe zs `bindMaybe` \m ->
                Just $ k + l+ m

-- Using prefix notation
fooPrefix' :: String -> String -> String -> Maybe Int
fooPrefix' xs ys zs = bindMaybe (readMaybe xs :: Maybe Int) (\k ->
                      bindMaybe (readMaybe ys :: Maybe Int) (\l ->
                      bindMaybe (readMaybe zs :: Maybe Int) (\m ->
                      Just $ k + l+ m )))           