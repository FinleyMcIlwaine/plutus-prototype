module Halogen.Monaco where

import Prelude

import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM, RefLabel(..))
import Halogen as H
import Halogen.HTML (HTML, div)
import Halogen.HTML.Properties (class_, ref)
import Marlowe.Linter as Linter
import Monaco (Monaco)
import Monaco as Monaco
import Monaco.Marlowe as MM

type State
  = { editor :: Maybe Monaco }

data Query a
  = Q a

data Action
  = Init

data Message
  = Initialized

monacoComponent :: forall m. MonadEffect m => H.Component HTML Query Unit Message m
monacoComponent =
  H.mkComponent
    { initialState: const { editor: Nothing }
    , render
    , eval:
      H.mkEval
        { handleAction
        , handleQuery
        , initialize: Just Init
        , receive: const Nothing
        , finalize: Nothing
        }
    }

render :: forall p i. State -> HTML p i
render state =
  div
    [ ref $ H.RefLabel "monacoEditor"
    , class_ $ H.ClassName "monaco-editor-container"
    ]
    []

handleAction :: forall slots m. MonadEffect m => Action -> HalogenM State Action slots Message m Unit
handleAction Init = do
  m <- liftEffect Monaco.getMonaco
  maybeElement <- H.getHTMLElementRef (RefLabel "monacoEditor")
  case maybeElement of
    Just element -> do
      trace element \_ -> pure unit
      liftEffect $ Monaco.registerLanguage m MM.languageExtensionPoint
      _ <- liftEffect $ Monaco.create m element (view MM._id MM.languageExtensionPoint) Linter.markers
      liftEffect $ Monaco.setMarloweTokensProvider m (view MM._id MM.languageExtensionPoint)
      liftEffect $ Monaco.registerCompletionItemProvider m (view MM._id MM.languageExtensionPoint) Linter.suggestions
      -- liftEffect $ Monaco.setMonarchTokensProvider m (view MM._id MM.languageExtensionPoint) MM.monarchLanguage
      _ <- H.modify (const { editor: Just m })
      pure unit
    Nothing -> pure unit
  H.raise Initialized

handleQuery :: forall a input m. Query a -> HalogenM State Action input Message m (Maybe a)
handleQuery (Q next) = pure $ Just next
