module Main where

import Prelude
import Concur.Core (Widget)
import Concur.Stage0 (runWidgetInDom)
import Concur.Stage0.DOM
import Concur.Stage0.Props as P

counterWidget :: ∀ a. Int -> Widget HTML a
counterWidget count = do
    n <- div'
        [ p' [text ("State: " <> show count)]
        , button [P.onClick] [text "Increment"] $> count+1
        , button [P.onClick] [text "Decrement"] $> count-1
        ]
    counterWidget n

main = runWidgetInDom "app" (counterWidget 0)
