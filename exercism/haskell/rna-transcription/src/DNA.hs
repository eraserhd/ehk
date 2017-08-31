module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = sequence . map rnaComplement

rnaComplement 'C' = Just 'G'
rnaComplement 'G' = Just 'C'
rnaComplement 'T' = Just 'A'
rnaComplement 'A' = Just 'U'
rnaComplement _   = Nothing
