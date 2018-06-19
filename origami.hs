-- | Fold over the input, folding left or right depending on the element.
origami :: (s -> l -> s) -> (r -> s -> s) -> s -> [Either l r] -> s
origami _ _ nil [] = nil
origami fl fr nil (x:xs) =
  case x of
    Left l -> origami fl fr (fl nil l) xs
    Right r -> fr r (origami fl fr nil xs)
