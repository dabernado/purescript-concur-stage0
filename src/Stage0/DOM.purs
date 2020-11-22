module Concur.Stage0.DOM where

import Prelude

import Concur.Core (class LiftWidget, Widget, liftWidget)
import Concur.Core.DOM (el, el', elLeaf) as CD
import Concur.Core.Props (Props)
import Concur.Core.Types (display)
import Concur.Stage0.Props (Prop)
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)

type HTML = Array VNode

data VNode
  = Content (Array Prop) String
  | Leaf String (Array Prop)
  | Node String (Array Prop) (Array VNode)
  | Empty

viewAdapter
  :: forall ps vs res
  .  (ps -> vs -> res)
  -> (ps -> vs -> Array res)
viewAdapter f = \ps vs -> [f ps vs]

el
  :: forall m a p v
  .  ShiftMap (Widget (Array v)) m
  => (Array p -> Array v -> v)
  -> Array (Props p a)
  -> m a
  -> m a
el f = CD.el (viewAdapter f)

el'
  :: forall m a p v
  .  ShiftMap (Widget (Array v)) m
  => MultiAlternative m
  => (Array p -> Array v -> v)
  -> Array (Props p a)
  -> Array (m a)
  -> m a
el' f = CD.el' (viewAdapter f)

elLeaf
  :: forall p v m a
  .  LiftWidget (Array v) m
  => (Array p -> v)
  -> Array (Props p a)
  -> m a
elLeaf f = CD.elLeaf (\ps -> [f ps])

text :: forall m a. LiftWidget HTML m => String -> m a
text str = liftWidget $ display $ [Content [] str]

type El
  = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array (Props Prop a) -> Array (m a) -> m a

type El'
  = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array (m a) -> m a

type Ell =
  forall m a. LiftWidget (Array VNode) m => Array (Props Prop a) -> m a

type Ell'
  = forall m a. LiftWidget HTML m => m a

html :: El
html = el' $ Node "html"

html' :: El'
html' = html []

style :: El
style = el' $ Node "style"

style' :: El'
style' = style []

script :: El
script = el' $ Node "script"

script' :: El'
script' = script []

a :: El
a = el' $ Node "a"

a' :: El'
a' = a []

div :: El
div = el' $ Node "div"

div' :: El'
div' = div []

button :: El
button = el' $ Node "button"

button' :: El'
button' = button []

body :: El
body = el' $ Node "body"

body' :: El'
body' = body []

head :: El
head = el' $ Node "head"

head' :: El'
head' = head []

meta :: Ell
meta = elLeaf $ Leaf "meta"

meta' :: Ell'
meta' = meta []

form :: El
form = el' $ Node "form"

form' :: El'
form' = form []

h1 :: El
h1 = el' $ Node "h1"

h1' :: El'
h1' = h1 []

h2 :: El
h2 = el' $ Node "h2"

h2' :: El'
h2' = h2 []

h3 :: El
h3 = el' $ Node "h3"

h3' :: El'
h3' = h3 []

h4 :: El
h4 = el' $ Node "h4"

h4' :: El'
h4' = h4 []

h5 :: El
h5 = el' $ Node "h5"

h5' :: El'
h5' = h5 []

h6 :: El
h6 = el' $ Node "h6"

h6' :: El'
h6' = h6 []

label :: El
label = el' $ Node "label"

label' :: El'
label' = label []

img :: Ell
img = elLeaf $ Leaf "img"

img' :: Ell'
img' = img []

input :: Ell
input = elLeaf $ Leaf "input"

input' :: Ell'
input' = input []

p :: El
p = el' $ Node "p"

p' :: El'
p' = p []

empty :: Ell
empty = elLeaf $ \ _ -> Empty
