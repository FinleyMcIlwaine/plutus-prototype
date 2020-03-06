module Halogen.Monaco where

import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM, RefLabel(..))
import Halogen as H
import Halogen.HTML (HTML, div)
import Halogen.HTML.Properties (class_, ref)
import Halogen.Query.EventSource (Emitter(..), Finalizer(..), effectEventSource)
import Marlowe.Linter as Linter
import Monaco (Editor, IPosition)
import Monaco as Monaco
import Monaco.Marlowe as MM
import Prelude (Unit, const, (>>=), discard, ($), bind, pure, unit)

type State
  = { editor :: Maybe Editor }

data Query a
  = SetText String a
  | GetText (String -> a)
  | SetPosition IPosition a

data Action
  = Init
  | HandleChange String

data Message
  = TextChanged String

monacoComponent :: forall m. MonadAff m => MonadEffect m => H.Component HTML Query Unit Message m
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

handleAction :: forall slots m. MonadAff m => MonadEffect m => Action -> HalogenM State Action slots Message m Unit
handleAction Init = do
  m <- liftEffect Monaco.getMonaco
  maybeElement <- H.getHTMLElementRef (RefLabel "monacoEditor")
  case maybeElement of
    Just element -> do
      liftEffect $ Monaco.registerLanguage m MM.languageExtensionPoint
      editor <- liftEffect $ Monaco.create m element (view MM._id MM.languageExtensionPoint) Linter.markers
      liftEffect $ Monaco.setMarloweTokensProvider m (view MM._id MM.languageExtensionPoint)
      liftEffect $ Monaco.registerCompletionItemProvider m (view MM._id MM.languageExtensionPoint) Linter.suggestions Linter.format Linter.provideCodeActions
      _ <- H.modify (const { editor: Just editor })
      _ <-
        H.subscribe
          $ effectEventSource (f1 editor)
      pure unit
    Nothing -> pure unit
  where
  f1 :: Editor -> Emitter Effect Action -> Effect (Finalizer Effect)
  f1 editor (Emitter emitter) = do
    Monaco.onDidChangeContent editor
      ( \_ -> do
          model <- Monaco.getModel editor
          emitter $ Left $ HandleChange $ Monaco.getValue model
      )
    pure $ Finalizer $ pure unit

handleAction (HandleChange contents) = H.raise $ TextChanged contents

handleQuery :: forall a input m. MonadEffect m => Query a -> HalogenM State Action input Message m (Maybe a)
handleQuery (SetText text next) = do
  H.gets _.editor
    >>= traverse_ \editor -> do
        model <- liftEffect $ Monaco.getModel editor
        liftEffect $ Monaco.setValue model text
  pure $ Just next

handleQuery (GetText f) = do
  H.gets _.editor
    >>= traverse
        ( \editor -> do
            model <- liftEffect $ Monaco.getModel editor
            let
              s = Monaco.getValue model
            pure $ f s
        )

handleQuery (SetPosition position next) = do
  H.gets _.editor
    >>= traverse_ \editor -> liftEffect $ Monaco.setPosition editor position
  pure $ Just next
