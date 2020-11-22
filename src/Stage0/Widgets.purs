module Concur.Stage0.Widgets where

{-
import Prelude
import Concur.Core (Widget)
import Concur.Core.Props (Props)
import Concur.Stage0.DOM (HTML)
import Concur.Stage0.DOM as D
import Concur.Stage0.Props (Prop)
import Concur.Stage0.Props as P

-- | A Text input that returns its contents on enter
textInputEnter ::
  String ->
  Boolean ->
  (forall a. Array (Props Prop a)) ->
  Widget HTML String
textInputEnter val reset props = do
  e <- D.input $ props <> [P.onSubmit, P.defaultValue val]
  -- HACK: Using forced do notation, to force evaluation of the text input value in the same handler
  new <- pure $ P.unsafeTargetValue e
  when reset $ liftEffect (P.resetTargetValue "" e)
  pure new
-}
