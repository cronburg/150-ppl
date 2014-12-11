type STMeasure a b = StateT a Measure b

-- Discards any number of cards, returning the number of cards discarded
cellarEffect' :: STMeasure Game Int 
cellarEffect' = do
  g  <- get 
  c' <- lift $ ((mayPick.p1) g) g CELLAR
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then discard c >> cellarEffect' >>= \n -> return $ n + 1 
               else return 0
    Nothing -> return 0

-- Discard any number of cards, then draw that many cards:
cellarEffect :: STMeasure Game ()  
cellarEffect = addActions 1 >> cellarEffect' >>= \n -> draw n

