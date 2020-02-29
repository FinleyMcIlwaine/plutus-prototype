module HaskellEditor where

import Classes (aHorizontal, accentBorderBottom, isActiveTab)
import Data.Lens (view)
import Data.Map as Map
import Editor (editorView)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.HTML (HTML, div, section, text)
import Halogen.HTML.Extra (mapComponent)
import Halogen.HTML.Properties (classes)
import Prelude (($), (<>))
import StaticData as StaticData
import Types (ChildSlots, FrontendState, HAction(..), View(..), _editorPreferences, _haskellEditorSlot)

render ::
  forall m.
  MonadAff m =>
  FrontendState ->
  ComponentHTML HAction ChildSlots m
render state =
  section [ classes [ ClassName "code-panel", ClassName "haskell-editor" ] ]
    [ mapComponent
        HaskellEditorAction
        $ editorView defaultContents _haskellEditorSlot StaticData.bufferLocalStorageKey editorPreferences
    ]
  where
  editorPreferences = view _editorPreferences state

  defaultContents = Map.lookup "Escrow" StaticData.demoFiles

bottomPanel :: forall p. FrontendState -> Array (HTML p HAction)
bottomPanel state =
  [ div [ classes ([ ClassName "footer-panel-bg" ] <> isActiveTab state HaskellEditor) ]
      [ section [ classes [ ClassName "panel-header", aHorizontal ] ]
          [ div [ classes [ ClassName "panel-sub-header-main", aHorizontal, accentBorderBottom ] ]
              [ div
                  [ classes ([ ClassName "panel-tab", aHorizontal ])
                  ]
                  [ text "Current State" ]
              ]
          ]
      , section
          [ classes [ ClassName "panel-sub-header", aHorizontal ]
          ]
          []
      ]
  ]
