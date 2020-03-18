module HaskellEditor where

import Classes (aHorizontal, accentBorderBottom, footerPanelBg, haskellEditor, isActiveTab)
import Data.Lens (to, view, (^.))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Editor (editorView)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.HTML (HTML, a, div, section, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Extra (mapComponent)
import Halogen.HTML.Properties (classes)
import Prelude (const, not, ($), (<<<), (<>))
import StaticData as StaticData
import Types (ChildSlots, FrontendState, HAction(..), View(..), _editorPreferences, _haskellEditorSlot, _showBottomPanel)

render ::
  forall m.
  MonadAff m =>
  FrontendState ->
  ComponentHTML HAction ChildSlots m
render state =
  section [ classes (haskellEditor state) ]
    [ mapComponent
        HaskellEditorAction
        $ editorView defaultContents _haskellEditorSlot StaticData.bufferLocalStorageKey editorPreferences
    ]
  where
  editorPreferences = view _editorPreferences state

  defaultContents = Map.lookup "Escrow" StaticData.demoFiles

bottomPanel :: forall p. FrontendState -> Array (HTML p HAction)
bottomPanel state =
  [ div [ classes (footerPanelBg state HaskellEditor <> isActiveTab state HaskellEditor) ]
      [ section [ classes [ ClassName "panel-header", aHorizontal ] ]
          [ div [ classes [ ClassName "panel-sub-header-main", aHorizontal, accentBorderBottom ] ]
              [ div
                  [ classes ([ ClassName "panel-tab", aHorizontal ])
                  ]
                  [ a [ onClick $ const $ Just $ ShowBottomPanel (state ^. _showBottomPanel <<< to not) ] [ text "X" ] ]
              ]
          ]
      , section
          [ classes [ ClassName "panel-sub-header", aHorizontal ]
          ]
          []
      ]
  ]
