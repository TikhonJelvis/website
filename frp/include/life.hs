let active  = accumB False $ not <$ pauses
    changes = whenE active (step <$ time) `union`
              (modify <$> clicks)
    life    = accumB (blank 100 100) changes

sink lifePanel [on paint :== renderLife <$> life]
reactimate $ repaint lifePanel <$ changes

let symb b = if b then "❚❚" else "▶"
sink pauseButton [text :== symb <$> active]
