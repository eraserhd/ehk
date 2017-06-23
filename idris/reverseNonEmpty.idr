%default total
reverseLemma : (x : a) -> (xs : List a) -> reverse (x :: xs) = reverse xs ++ [x]
reverseLemma x xs = ?reverseLemma_rhs
-- reverseLemma x [] = Refl

appendRightNonEmpty : (l1, l2 : List a) -> (NonEmpty l2) -> NonEmpty (l1 ++ l2)
appendRightNonEmpty [] (x :: xs) IsNonEmpty = IsNonEmpty
appendRightNonEmpty (y :: ys) _ IsNonEmpty = IsNonEmpty

nonEmptyReverse : (l : List a) -> NonEmpty l -> NonEmpty (reverse l)
nonEmptyReverse (x1 :: xs) IsNonEmpty =
  rewrite reverseLemma x1 xs in
  appendRightNonEmpty (reverse xs) [x1] IsNonEmpty

