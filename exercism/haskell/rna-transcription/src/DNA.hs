module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = mapM rnaComplement

rnaComplement 'C' = Just 'G'
rnaComplement 'G' = Just 'C'
rnaComplement 'T' = Just 'A'
rnaComplement 'A' = Just 'U'
rnaComplement _   = Nothing
