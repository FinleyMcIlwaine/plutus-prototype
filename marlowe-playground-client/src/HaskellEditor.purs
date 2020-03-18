module HaskellEditor where

import Bootstrap (btn, btnPrimary, listGroupItem_, listGroup_)
import Classes (aHorizontal, accentBorderBottom, closeDrawerIcon, footerPanelBg, haskellEditor, isActiveTab, minimizeIcon)
import Data.Either (Either(..))
import Data.Json.JsonEither (JsonEither(..))
import Data.Lens (to, view, (^.))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Editor (editorView)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.HTML (HTML, a, button, code_, div, div_, img, pre, section, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Extra (mapComponent)
import Halogen.HTML.Properties (alt, class_, classes, disabled, src)
import Language.Haskell.Interpreter (InterpreterResult(..))
import Network.RemoteData (RemoteData(..), isLoading, isSuccess)
import Prelude (const, not, ($), (<<<), (<>), (||))
import StaticData as StaticData
import Types (ChildSlots, FrontendState, HAction(..), View(..), _compilationResult, _editorPreferences, _haskellEditorSlot, _showBottomPanel)

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
                  [ button [ onClick $ const $ Just CompileHaskellProgram ] [ text "Compile" ]
                  , a [ onClick $ const $ Just $ ShowBottomPanel (state ^. _showBottomPanel <<< to not) ]
                      [ img [ classes (minimizeIcon state), src closeDrawerIcon, alt "close drawer icon" ] ]
                  ]
              ]
          ]
      , section
          [ classes [ ClassName "panel-sub-header", aHorizontal ]
          ]
          [ resultPane state ]
      ]
  ]

resultPane :: forall p. FrontendState -> HTML p HAction
resultPane state =
  let
    compilationResult = view _compilationResult state
  in
    case compilationResult of
      Success (JsonEither (Right (InterpreterResult result))) ->
        listGroup_
          [ listGroupItem_
              [ div_
                  [ button
                      [ classes
                          [ btn
                          , btnPrimary
                          , ClassName "float-right"
                          ]
                      , onClick $ const $ Just SendResult
                      , disabled (isLoading compilationResult || (not isSuccess) compilationResult)
                      ]
                      [ text "Send to Simulator" ]
                  , code_
                      [ pre [ class_ $ ClassName "success-code" ] [ text (unwrap result.result) ]
                      ]
                  ]
              ]
          ]
      _ -> text ""
