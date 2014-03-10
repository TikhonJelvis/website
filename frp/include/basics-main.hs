main = start $ do
  mw          <- frame     [text := "Life", resizeable := False]
  lifeTimer   <- timer  mw [interval := 50]
  lifePanel   <- panel  mw [bgcolor := white]
  pauseButton <- button mw [text := "▶"]
