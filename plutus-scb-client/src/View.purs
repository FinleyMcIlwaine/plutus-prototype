module View (render) where

import Effect.Aff.Class (class MonadAff)
import Halogen.HTML (ComponentHTML, code_, div_, text)
import Prelude
import Types (HAction, State)

render ::
  forall m slots.
  MonadAff m =>
  State -> ComponentHTML HAction slots m
render state =
  div_
    [ code_ [ text $ show state ]
    ]
