module Concur.Stage0 where

import Prelude

import Concur.Core.Discharge (dischargePartialEffect)
import Concur.Core.Types (Widget)
import Concur.Stage0.DOM (VNode(..), HTML)
import Concur.Stage0.Props (Prop(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (foldl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Web.DOM (Document, Element, Node)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode)
import Web.HTML.Window (document)

foreign import _h :: String -> Node
--foreign import collect :: Node -> Array Node

foreign import _reconcile :: forall a. EffectFn4
  Element
  (Array a)
  (Array a)
  (VNode -> Node)
  Unit

reconcile :: forall a.
    Element
 -> Array a
 -> Array a
 -> (VNode -> Node)
 -> Effect Unit
reconcile = runEffectFn4 _reconcile

renderStringEl :: VNode -> String
renderStringEl (Content ps s) = s
renderStringEl Empty = ""
renderStringEl (Leaf n []) = "<" <> n <> "/>"
renderStringEl (Leaf n ps) =
  "<" <> n <> " " <> (renderPropsString ps) <> "/>"
renderStringEl (Node n [] vs) =
  "<" <> n <> ">" <>
    (foldl (<>) "" $ map renderStringEl vs) <> "</" <> n <> ">"
renderStringEl (Node n ps vs) =
  "<" <> n <> " " <> (renderPropsString ps) <> ">" <>
    (foldl (<>) "" $ map renderStringEl vs) <> "</" <> n <> ">"

renderPropsString :: Array Prop -> String
renderPropsString = joinWith " " <<< map renderPropString
  where
    renderPropString (Primitive k v) = k <> "=" <> "\"" <> v <> "\""
    renderPropString (PHandler _ _) = ""

renderNode :: VNode -> Node
renderNode v = _h $ renderStringEl v

runWidgetInDom :: ∀ a. String -> Widget HTML a -> Effect Unit
runWidgetInDom elemId winit = do
  win <- window
  d <- document win
  let doc = toDocument d
  let node = toNonElementParentNode d
  mroot <- getElementById elemId node
  case mroot of
    Nothing -> pure unit
    Just root -> run root doc
  where
    run :: Element -> Document -> Effect Unit
    run node doc = do
       winit' /\ new <- dischargePartialEffect winit
       void $ reconcile node [] new renderNode
