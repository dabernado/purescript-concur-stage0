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
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Web.DOM (Document, Element, Node)
import Web.DOM.Element (toNode)
import Web.DOM.Node (childNodes)
import Web.DOM.NodeList (toArray)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode)
import Web.HTML.Window (document)

--foreign import h :: String -> Node
--foreign import collect :: Node -> Array Node

foreign import outerHTML :: Node -> String
foreign import _reconcile :: forall a. EffectFn3
  Element
  (Array a)
  (Array a)
  Unit

reconcile :: forall a.
    Element
 -> Array a
 -> Array a
 -> Effect Unit
reconcile = runEffectFn3 _reconcile

renderString :: Array VNode -> Array String
renderString = map renderStringEl
  where
    renderStringEl (Content ps s) = s
    renderStringEl Empty = ""
    renderStringEl (Leaf n []) = "<" <> n <> "/>"
    renderStringEl (Leaf n ps) =
      "<" <> n <> " " <> (renderPropsString ps) <> "/>"
    renderStringEl (Node n [] vs) =
      "<" <> n <> ">" <>
        (foldl (<>) "" $ renderString vs) <> "</" <> n <> ">"
    renderStringEl (Node n ps vs) =
      "<" <> n <> " " <> (renderPropsString ps) <> ">" <>
        (foldl (<>) "" $ renderString vs) <> "</" <> n <> ">"
    renderPropsString = joinWith " " <<< map renderPropString
    renderPropString (Primitive k v) = k <> "=" <> "\"" <> v <> "\""
    renderPropString (PHandler _ _) = ""

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
       children <- childNodes $ toNode node
       cs <- toArray children
       let old = map outerHTML cs
       winit' /\ v <- dischargePartialEffect winit
       let new = renderString v
       void $ reconcile node old new
