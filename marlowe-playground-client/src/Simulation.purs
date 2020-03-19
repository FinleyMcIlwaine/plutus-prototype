module Simulation where

import Ace.Halogen.Component (Autocomplete(Live), aceComponent)
import Auth (AuthRole(..), authStatusAuthRole)
import Classes (aHorizontal, accentBorderBottom, active, activeTextPrimary, blocklyIcon, bold, closeDrawerIcon, codeEditor, downloadIcon, first, flex, flexLeft, flexTen, footerPanelBg, githubDisplay, infoIcon, isActiveDemo, isActiveTab, jFlexStart, minimizeIcon, minusBtn, noMargins, panelHeader, panelHeaderMain, panelHeaderSide, panelSubHeader, panelSubHeaderMain, panelSubHeaderSide, plusBtn, pointer, rTable, rTable6cols, rTableCell, rTableEmptyRow, simulationBottomPanel, smallBtn, spaceLeft, textSecondaryColor, uppercase)
import Classes as Classes
import Control.Alternative (map)
import Data.Array (concatMap, intercalate, length)
import Data.Array as Array
import Data.BigInteger (BigInteger, fromString, fromInt)
import Data.Either (Either(..))
import Data.Eq (eq, (==))
import Data.Foldable (foldMap)
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Lens (to, view, (^.))
import Data.List (List, toUnfoldable)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (uncurry5, (/\), type (/\))
import Editor (initEditor) as Editor
import Effect.Aff.Class (class MonadAff)
import Gist (Gist(..))
import Gists (GistAction(..), idPublishGist)
import Halogen.HTML (ClassName(..), ComponentHTML, HTML, a, a_, article, aside, b_, button, code_, div, em_, h2, h3_, h4, h6, h6_, img, input, label, li, li_, ol, ol_, p, p_, pre, section, slot, small, small_, span, span_, strong_, text, ul, ul_)
import Halogen.HTML.Events (onClick, onValueChange, onValueInput)
import Halogen.HTML.Properties (InputType(..), alt, class_, classes, disabled, enabled, href, placeholder, src, type_, value)
import Halogen.HTML.Properties as HTML
import Halogen.SVG (Box(..), Length(..), Linecap(..), RGB(..), circle, clazz, cx, cy, d, fill, height, path, r, strokeLinecap, strokeWidth, svg, viewBox)
import Halogen.SVG as SVG
import Help (toHTML)
import Icons (Icon(..), icon)
import Marlowe.Parser (transactionInputList, transactionWarningList)
import Marlowe.Semantics (AccountId(..), Assets(..), Bound(..), ChoiceId(..), Input(..), Party, Payee(..), Payment(..), PubKey, Slot(..), SlotInterval(..), Token(..), TransactionError, TransactionInput(..), TransactionWarning(..), ValueId(..), _accounts, _boundValues, _choices, inBounds, maxTime)
import Marlowe.Symbolic.Types.Response as R
import Network.RemoteData (RemoteData(..), isLoading)
import Prelude (class Show, Unit, bind, const, mempty, pure, show, unit, zero, ($), (<$>), (<<<), (<>), (>), (/=))
import Servant.PureScript.Ajax (AjaxError(..))
import StaticData as StaticData
import Text.Parsing.StringParser (runParser)
import Types (ActionInput(..), ActionInputId, ChildSlots, FrontendState, HAction(..), HelpContext(..), SimulationBottomPanelView(..), View(..), _Head, _analysisState, _authStatus, _contract, _createGistResult, _editorErrors, _editorPreferences, _editorWarnings, _helpContext, _loadGistResult, _marloweEditorSlot, _marloweState, _payments, _pendingInputs, _possibleActions, _showBottomPanel, _simulationBottomPanelView, _slot, _state, _transactionError, _transactionWarnings)

isContractValid :: FrontendState -> Boolean
isContractValid state =
  view (_marloweState <<< _Head <<< _contract) state /= Nothing
    && view (_marloweState <<< _Head <<< _editorErrors) state
    == []

render ::
  forall m.
  MonadAff m =>
  FrontendState ->
  Array (ComponentHTML HAction ChildSlots m)
render state =
  [ section [ classes [ panelHeader, aHorizontal ] ]
      [ div [ classes [ panelHeaderMain, aHorizontal, noMargins, accentBorderBottom ] ]
          [ h4 [] [ text "Marlowe Contract" ] ]
      , div [ classes [ panelHeaderSide, aHorizontal, accentBorderBottom ] ]
          [ div [ classes ([ ClassName "vertical", ClassName "flip-container" ] <> githubDisplay state) ]
              [ authButton state
              ]
          ]
      ]
  , section [ classes [ panelSubHeader, aHorizontal ] ]
      [ div [ classes [ panelSubHeaderMain, aHorizontal ] ]
          [ div [ classes [ ClassName "demo-title", aHorizontal, jFlexStart ] ]
              [ img [ class_ (ClassName "demo-arrow"), src downloadIcon, alt "down arrow" ]
              , div [ classes [ ClassName "demos", spaceLeft ] ]
                  [ small_ [ text "Demos:" ]
                  ]
              ]
          , ul [ classes [ ClassName "demo-list", aHorizontal ] ]
              (demoScriptLink <$> Array.fromFoldable (Map.keys StaticData.marloweContracts))
          , div [ class_ (ClassName "code-to-blockly-wrap") ]
              [ button
                  [ class_ smallBtn
                  , onClick $ const $ Just $ SetBlocklyCode
                  , enabled (isContractValid state)
                  ]
                  [ img [ class_ (ClassName "blockly-btn-icon"), src blocklyIcon, alt "blockly logo" ] ]
              ]
          ]
      , div [ classes [ panelSubHeaderSide ] ] []
      ]
  , section [ class_ (ClassName "code-panel") ]
      [ div [ classes (codeEditor state) ]
          [ marloweEditor state ]
      , sidebar state
      ]
  ]
  where
  demoScriptLink key = li [ classes (isActiveDemo state) ] [ a [ onClick $ const $ Just $ LoadMarloweScript key ] [ text key ] ]

gist :: forall p. FrontendState -> HTML p HAction
gist state =
  div [ classes [ ClassName "github-gist-panel", aHorizontal ] ]
    [ div [ classes [ ClassName "input-group-text", ClassName "upload-btn" ], onClick $ const $ Just $ GistAction PublishGist ]
        [ svg [ SVG.style "width:20px;height:20px", SVG.viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
            [ path [ fill (Hex "#832dc4"), d "M4.08,11.92L12,4L19.92,11.92L18.5,13.33L13,7.83V22H11V7.83L5.5,13.33L4.08,11.92M12,4H22V2H2V4H12Z" ] [] ]
        ]
    , label [ classes [ ClassName "sr-only", active ], HTML.for "github-input" ] [ text "Enter Github Gist" ]
    , div [ classes (map ClassName [ "input-group", "mb-2", "mr-sm-2" ]) ]
        [ gistInput loadStatus
        , div [ class_ (ClassName "input-group-append") ]
            [ div [ class_ (ClassName "input-group-text"), onClick $ const $ Just $ GistAction LoadGist ]
                [ loadGistButton loadStatus
                ]
            ]
        ]
    ]
  where
  publishStatus = state ^. _createGistResult

  loadStatus = state ^. _loadGistResult

marloweEditor ::
  forall m.
  MonadAff m =>
  FrontendState ->
  ComponentHTML HAction ChildSlots m
marloweEditor state = slot _marloweEditorSlot unit component unit (Just <<< MarloweHandleEditorMessage)
  where
  component = aceComponent (Editor.initEditor initialContents StaticData.marloweBufferLocalStorageKey editorPreferences) (Just Live)

  initialContents = Map.lookup "Deposit Incentive" StaticData.marloweContracts

  editorPreferences = view _editorPreferences state

sidebar ::
  forall p.
  FrontendState ->
  HTML p HAction
sidebar state =
  aside [ class_ (ClassName "sidebar-composer") ]
    [ div [ class_ aHorizontal ]
        [ h6 [ classes [ ClassName "input-composer-heading", noMargins ] ]
            [ small [ classes [ textSecondaryColor, bold, uppercase ] ] [ text "Input Composer" ] ]
        , a [ onClick $ const $ Just $ ChangeHelpContext InputComposerHelp ] [ img [ src infoIcon, alt "info book icon" ] ]
        ]
    , inputComposer state
    , div [ class_ aHorizontal ]
        [ h6 [ classes [ ClassName "input-composer-heading", noMargins ] ]
            [ small [ classes [ textSecondaryColor, bold, uppercase ] ] [ text "Transaction Composer" ] ]
        , a [ onClick $ const $ Just $ ChangeHelpContext TransactionComposerHelp ] [ img [ src infoIcon, alt "info book icon" ] ]
        ]
    , transactionComposer state
    , article [ class_ (ClassName "documentation-panel") ]
        (toHTML (state ^. _helpContext))
    ]

inputComposer ::
  forall p.
  FrontendState ->
  HTML p HAction
inputComposer state =
  div [ class_ (ClassName "input-composer") ]
    [ ul [ class_ (ClassName "participants") ]
        if (Map.isEmpty possibleActions) then
          [ text "No valid inputs can be added to the transaction" ]
        else
          (actionsForPeople possibleActions)
    ]
  where
  isEnabled = isContractValid state

  possibleActions = view (_marloweState <<< _Head <<< _possibleActions) state

  kvs :: forall k v. Map k v -> Array (Tuple k v)
  kvs = Map.toUnfoldable

  vs :: forall k v. Map k v -> Array v
  vs m = map snd (kvs m)

  lastKey :: Maybe (Maybe PubKey)
  lastKey = map (\x -> x.key) (Map.findMax possibleActions)

  actionsForPeople :: Map (Maybe PubKey) (Map ActionInputId ActionInput) -> Array (HTML p HAction)
  actionsForPeople m = map (\(Tuple k v) -> participant isEnabled k (vs v)) (kvs m)

participant ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  Array ActionInput ->
  HTML p HAction
participant isEnabled person actionInputs =
  li [ classes [ ClassName "participant-a", noMargins ] ]
    [ h6_ [ em_ [ text "Participant ", strong_ [ text "'Alice'" ] ] ]
    , ul
        []
        (map (inputItem isEnabled person) actionInputs)
    ]

inputItem ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  ActionInput ->
  HTML p HAction
inputItem isEnabled person (DepositInput accountId party token value) =
  li [ classes [ ClassName "deposit-a", aHorizontal ] ]
    [ button
        [ classes [ plusBtn, smallBtn ]
        , enabled isEnabled
        , onClick $ const $ Just
            $ AddInput person (IDeposit accountId party token value) []
        ]
        [ text "+" ]
    , p_ (renderDeposit accountId party token value)
    ]

inputItem isEnabled person (ChoiceInput choiceId@(ChoiceId choiceName choiceOwner) bounds chosenNum) =
  li
    [ classes [ ClassName "choice-a", aHorizontal ] ]
    [ button
        [ classes [ plusBtn, smallBtn ]
        , enabled (isEnabled && inBounds chosenNum bounds)
        , onClick $ const $ Just
            $ AddInput person (IChoice (ChoiceId choiceName choiceOwner) chosenNum) bounds
        ]
        [ text "+" ]
    , p [ class_ (ClassName "choice-input") ]
        [ spanText "Choice "
        , b_ [ spanText (show choiceName) ]
        , spanText ": Choose value "
        , marloweActionInput isEnabled (SetChoice choiceId) chosenNum
        ]
    , p_ error
    ]
  where
  error = if inBounds chosenNum bounds then [] else [ text boundsError ]

  boundsError = "Choice must be between " <> intercalate " or " (map boundError bounds)

  boundError (Bound from to) = show from <> " and " <> show to

inputItem isEnabled person NotifyInput =
  li
    [ classes [ ClassName "choice-a", aHorizontal ] ]
    [ button
        [ classes [ plusBtn, smallBtn ]
        , enabled isEnabled
        , onClick $ const $ Just
            $ AddInput person INotify []
        ]
        [ text "+" ]
    , p_ [ text "Notify Contract" ]
    ]

marloweActionInput :: forall p a. Show a => Boolean -> (BigInteger -> HAction) -> a -> HTML p HAction
marloweActionInput isEnabled f current =
  input
    [ type_ InputNumber
    , enabled isEnabled
    , placeholder "BigInteger"
    , class_ $ ClassName "action-input"
    , value $ show current
    , onValueChange
        $ ( \x ->
              Just
                $ f
                    ( case fromString x of
                        Just y -> y
                        Nothing -> fromInt 0
                    )
          )
    ]

renderDeposit :: forall p. AccountId -> Party -> Token -> BigInteger -> Array (HTML p HAction)
renderDeposit (AccountId accountNumber accountOwner) party tok money =
  [ spanText "Deposit "
  , b_ [ spanText (show money) ]
  , spanText " units of "
  , b_ [ spanText (show tok) ]
  , spanText " into Account "
  , b_ [ spanText (show accountOwner <> " (" <> show accountNumber <> ")") ]
  , spanText " as "
  , b_ [ spanText (show party) ]
  ]

transactionComposer ::
  forall p.
  FrontendState ->
  HTML p HAction
transactionComposer state =
  div [ class_ (ClassName "input-composer") ]
    [ ul [ class_ (ClassName "participants") ]
        [ transaction state ]
    , div [ class_ (ClassName "transaction-btns") ]
        [ ul [ classes [ ClassName "demo-list", aHorizontal ] ]
            [ li [ classes [ activeTextPrimary, bold, pointer ] ]
                [ a
                    [ onClick
                        $ if hasHistory state then
                            Just <<< const Undo
                          else
                            const Nothing
                    ]
                    [ text "Undo" ]
                ]
            , li [ classes [ activeTextPrimary, bold, pointer ] ]
                [ a
                    [ onClick
                        $ if hasHistory state then
                            Just <<< const ResetSimulator
                          else
                            const Nothing
                    ]
                    [ text "Reset" ]
                ]
            , li [ classes [ activeTextPrimary, bold, pointer ] ]
                [ a
                    [ onClick
                        $ if isContractValid state then
                            Just <<< const NextSlot
                          else
                            const Nothing
                    ]
                    [ text $ "Next Block (" <> show currentBlock <> ")" ]
                ]
            , li_
                [ button
                    [ onClick $ Just <<< const ApplyTransaction
                    , enabled $ isContractValid state
                    ]
                    [ text "Apply" ]
                ]
            ]
        ]
    ]
  where
  currentBlock = state ^. (_marloweState <<< _Head <<< _slot)

transaction ::
  forall p.
  FrontendState ->
  HTML p HAction
transaction state =
  li [ classes [ ClassName "participant-a", noMargins ] ]
    [ ul
        []
        (map (transactionRow state isEnabled) (state ^. (_marloweState <<< _Head <<< _pendingInputs)))
    ]
  where
  isEnabled = state ^. (_marloweState <<< _Head <<< _contract) /= Nothing || state ^. (_marloweState <<< _Head <<< _editorErrors) /= []

transactionRow ::
  forall p.
  FrontendState ->
  Boolean ->
  Tuple Input (Maybe PubKey) ->
  HTML p HAction
transactionRow state isEnabled (Tuple input@(IDeposit (AccountId accountNumber accountOwner) party token money) person) =
  li [ classes [ ClassName "choice-a", aHorizontal ] ]
    [ p_
        [ text "Deposit"
        , strong_ [ text (show money) ]
        , text " units of "
        , strong_ [ text (show token) ]
        , text " into account "
        , strong_ [ text (show accountOwner <> " (" <> show accountNumber <> ")") ]
        , text " as "
        , strong_ [ text (show party) ]
        ]
    , button
        [ classes [ minusBtn, smallBtn, bold ]
        , enabled isEnabled
        , onClick $ const $ Just $ RemoveInput person input
        ]
        [ text "-" ]
    ]

transactionRow state isEnabled (Tuple input@(IChoice (ChoiceId choiceName choiceOwner) chosenNum) person) =
  li [ classes [ ClassName "choice-a", aHorizontal ] ]
    [ p_
        [ text "Participant"
        , strong_ [ text (show choiceOwner) ]
        , text " chooses the value "
        , strong_ [ text (show chosenNum) ]
        , text " for choice with id "
        , strong_ [ text (show choiceName) ]
        ]
    , button
        [ classes [ minusBtn, smallBtn, bold ]
        , enabled isEnabled
        , onClick $ const $ Just $ RemoveInput person input
        ]
        [ text "-" ]
    ]

transactionRow state isEnabled (Tuple INotify person) =
  li [ classes [ ClassName "choice-a", aHorizontal ] ]
    [ p_
        [ text "Notification"
        ]
    , button
        [ classes [ minusBtn, smallBtn, bold ]
        , enabled isEnabled
        , onClick $ const $ Just $ RemoveInput person INotify
        ]
        [ text "-" ]
    ]

bottomPanel :: forall p. FrontendState -> HTML p HAction
bottomPanel state =
  div [ classes (simulationBottomPanel state) ]
    [ div [ class_ flex ]
        [ div [ class_ flexTen ]
            [ div [ classes (footerPanelBg state Simulation <> isActiveTab state Simulation) ]
                [ section [ classes [ ClassName "panel-header", aHorizontal ] ]
                    [ div [ classes [ ClassName "panel-sub-header-main", aHorizontal, accentBorderBottom ] ]
                        [ ul [ classes [ ClassName "demo-list", aHorizontal ] ]
                            [ li
                                [ classes ((if hasRuntimeWarnings || hasRuntimeError then [ ClassName "error-tab" ] else []) <> isActive CurrentStateView)
                                , onClick $ const $ Just $ ChangeSimulationView CurrentStateView
                                ]
                                [ text "Current State" ]
                            , li
                                [ classes ([] <> isActive StaticAnalysisView)
                                , onClick $ const $ Just $ ChangeSimulationView StaticAnalysisView
                                ]
                                [ text "Static Analysis" ]
                            , li
                                [ classes ([] <> isActive MarloweWarningsView)
                                , onClick $ const $ Just $ ChangeSimulationView MarloweWarningsView
                                ]
                                [ text $ "Warnings" <> if warnings == [] then "" else " (" <> show (length warnings) <> ")" ]
                            , li
                                [ classes ([] <> isActive MarloweErrorsView)
                                , onClick $ const $ Just $ ChangeSimulationView MarloweErrorsView
                                ]
                                [ a_ [ text $ "Errors" <> if errors == [] then "" else " (" <> show (length errors) <> ")" ] ]
                            ]
                        , ul [ classes [ ClassName "end-item", aHorizontal ] ]
                            [ li [ classes [ Classes.stateLabel ] ]
                                [ text "Contract Expiration: ", state ^. (_marloweState <<< _Head <<< _contract <<< to contractMaxTime <<< to text) ]
                            , li [ classes [ ClassName "space-left", Classes.stateLabel ] ]
                                [ text "Current Blocks: ", state ^. (_marloweState <<< _Head <<< _slot <<< to show <<< to text) ]
                            , li [ class_ (ClassName "space-left") ]
                                [ a [ onClick $ const $ Just $ ShowBottomPanel (state ^. _showBottomPanel <<< to not) ]
                                    [ img [ classes (minimizeIcon state), src closeDrawerIcon, alt "close drawer icon" ] ]
                                ]
                            ]
                        ]
                    ]
                , panelContents state (state ^. _simulationBottomPanelView)
                ]
            ]
        ]
    ]
  where
  isActive view = if state ^. _simulationBottomPanelView <<< (to (eq view)) then [ ClassName "active-text" ] else []

  warnings = state ^. (_marloweState <<< _Head <<< _editorWarnings)

  errors = state ^. (_marloweState <<< _Head <<< _editorErrors)

  hasRuntimeWarnings = state ^. (_marloweState <<< _Head <<< _transactionWarnings <<< to Array.null <<< to not)

  hasRuntimeError = state ^. (_marloweState <<< _Head <<< _transactionError <<< to isJust)

  contractMaxTime Nothing = "Closed"

  contractMaxTime (Just contract) = let t = maxTime contract in if t == zero then "Closed" else show t

panelContents :: forall p. FrontendState -> SimulationBottomPanelView -> HTML p HAction
panelContents state CurrentStateView =
  div [ classes [ rTable, rTable6cols, Classes.panelContents ] ]
    ( warningsRow <> errorRow
        <> tableRow "Accounts" "No accounts have been used" "Account ID" "Participant" "Currency Symbol" "Token Name" "Money"
            accountsData
        <> tableRow "Choices" "No Choices have been made" "Choice ID" "Participant" "Chosen Value" "" ""
            choicesData
        <> tableRow "Payments" "No payments have been recorded" "Party" "Currency Symbol" "Token Name" "Money" "" paymentsData
        <> tableRow "Let Bindings" "No values have been bound" "Identifier" "Value" "" "" "" bindingsData
    )
  where
  warnings = state ^. (_marloweState <<< _Head <<< _transactionWarnings)

  warningsRow =
    if Array.null warnings then
      []
    else
      (headerRow "Warnings" "type" "details" "" "" "") <> foldMap displayWarning' warnings

  error = state ^. (_marloweState <<< _Head <<< _transactionError)

  errorRow =
    if isNothing error then
      []
    else
      (headerRow "Errors" "type" "details" "" "" "") <> displayError error

  displayError Nothing = []

  displayError (Just err) = [ div [ class_ (ClassName "RTable-5-cells") ] [ text $ show err ] ]

  accountsData =
    let
      (accounts :: Array _) = state ^. (_marloweState <<< _Head <<< _state <<< _accounts <<< to Map.toUnfoldable)

      asTuple (Tuple (Tuple (AccountId accountNumber accountOwner) (Token currSym tokName)) value) = show accountNumber /\ show accountOwner /\ show currSym /\ show tokName /\ show value /\ unit
    in
      map asTuple accounts

  choicesData =
    let
      (choices :: Array _) = state ^. (_marloweState <<< _Head <<< _state <<< _choices <<< to Map.toUnfoldable)

      asTuple (Tuple (ChoiceId choiceName choiceOwner) value) = show choiceName /\ show choiceOwner /\ show value /\ mempty /\ mempty /\ unit
    in
      map asTuple choices

  paymentsData =
    let
      payments = state ^. (_marloweState <<< _Head <<< _payments)

      asTuple :: Payment -> Array (String /\ String /\ String /\ String /\ String /\ Unit)
      asTuple (Payment party (Assets mon)) =
        concatMap
          ( \(Tuple currencySymbol tokenMap) ->
              ( map
                  ( \(Tuple tokenName value) ->
                      (show party /\ currencySymbol /\ tokenName /\ show value /\ mempty /\ unit)
                  )
                  (Map.toUnfoldable tokenMap)
              )
          )
          (Map.toUnfoldable mon)
    in
      concatMap asTuple payments

  bindingsData =
    let
      (bindings :: Array _) = state ^. (_marloweState <<< _Head <<< _state <<< _boundValues <<< to Map.toUnfoldable)

      asTuple (Tuple (ValueId valueId) value) = show valueId /\ show value /\ mempty /\ mempty /\ mempty /\ unit
    in
      map asTuple bindings

  tableRow title emptyMessage a b c d e [] = emptyRow title emptyMessage

  tableRow title emptyMessage a b c d e rowData = headerRow title a b c d e <> foldMap (\dataTuple -> uncurry5 row dataTuple) rowData

  headerRow title a b c d e =
    [ div [ classes [ rTableCell, first, Classes.header ] ] [ text title ]
    , div [ classes [ rTableCell, rTableCell, Classes.header ] ] [ text a ]
    , div [ classes [ rTableCell, rTableCell, Classes.header ] ] [ text b ]
    , div [ classes [ rTableCell, rTableCell, Classes.header ] ] [ text c ]
    , div [ classes [ rTableCell, rTableCell, Classes.header ] ] [ text d ]
    , div [ classes [ rTableCell, rTableCell, Classes.header ] ] [ text e ]
    ]

  row a b c d e =
    [ div [ classes [ rTableCell, first ] ] []
    , div [ class_ rTableCell ] [ text a ]
    , div [ class_ rTableCell ] [ text b ]
    , div [ class_ rTableCell ] [ text c ]
    , div [ class_ rTableCell ] [ text d ]
    , div [ class_ rTableCell ] [ text e ]
    ]

  emptyRow title message =
    [ div [ classes [ rTableCell, first, Classes.header ] ]
        [ text title ]
    , div [ classes [ rTableCell, rTableEmptyRow, Classes.header ] ] [ text message ]
    ]

  displayWarning' (TransactionNonPositiveDeposit party (AccountId accNum owner) tok amount) =
    [ div [ classes [ rTableCell, first ] ] []
    , div [ class_ (ClassName "RTable-2-cells") ] [ text "TransactionNonPositiveDeposit" ]
    , div [ class_ (ClassName "RTable-4-cells") ]
        [ text $ "Party " <> show party <> " is asked to deposit " <> show amount
            <> " units of "
            <> show tok
            <> " into account "
            <> show accNum
            <> " of "
            <> show owner
            <> "."
        ]
    ]

  displayWarning' (TransactionNonPositivePay (AccountId accNum owner) payee tok amount) =
    [ div [ classes [ rTableCell, first ] ] []
    , div [ class_ (ClassName "RTable-2-cells") ] [ text "TransactionNonPositivePay" ]
    , div [ class_ (ClassName "RTable-4-cells") ]
        [ text $ "The contract is supposed to make a payment of "
            <> show amount
            <> " units of "
            <> show tok
            <> " from account "
            <> show accNum
            <> " of "
            <> show owner
            <> " to "
            <> ( case payee of
                  (Account (AccountId accNum2 owner2)) -> "account " <> show accNum2 <> " of " <> show owner2
                  (Party dest) -> "party " <> show dest
              )
            <> "."
        ]
    ]

  displayWarning' (TransactionPartialPay (AccountId accNum owner) payee tok amount expected) =
    [ div [ classes [ rTableCell, first ] ] []
    , div [ class_ (ClassName "RTable-2-cells") ] [ text "TransactionPartialPay" ]
    , div [ class_ (ClassName "RTable-4-cells") ]
        [ text $ "The contract is supposed to make a payment of "
            <> show expected
            <> " units of "
            <> show tok
            <> " from account "
            <> show accNum
            <> " of "
            <> show owner
            <> " to "
            <> ( case payee of
                  (Account (AccountId accNum2 owner2)) -> ("account " <> show accNum2 <> " of " <> show owner2)
                  (Party dest) -> ("party " <> show dest)
              )
            <> " but there is only "
            <> show amount
            <> "."
        ]
    ]

  displayWarning' (TransactionShadowing valId oldVal newVal) =
    [ div [ classes [ rTableCell, first ] ] []
    , div [ class_ (ClassName "RTable-2-cells") ] [ text "TransactionShadowing" ]
    , div [ class_ (ClassName "RTable-4-cells") ]
        [ text $ "The contract defined the value with id "
            <> show valId
            <> " before, it was assigned the value "
            <> show oldVal
            <> " and now it is being assigned the value "
            <> show newVal
            <> "."
        ]
    ]

panelContents state StaticAnalysisView =
  section
    [ classes [ ClassName "panel-sub-header", aHorizontal, Classes.panelContents ]
    ]
    [ analysisResultPane state
    , button [ onClick $ const $ Just $ AnalyseContract, enabled (state ^. _analysisState <<< to isLoading <<< to not) ] [ text "Analyse" ]
    ]

panelContents state MarloweWarningsView =
  section
    [ classes [ ClassName "panel-sub-header", aHorizontal, Classes.panelContents, flexLeft ]
    ]
    (map renderWarning (state ^. (_marloweState <<< _Head <<< _editorWarnings)))
  where
  renderWarning warning = pre [ class_ (ClassName "warning-content") ] [ text warning.text ]

panelContents state MarloweErrorsView =
  section
    [ classes [ ClassName "panel-sub-header", aHorizontal, Classes.panelContents, flexLeft ]
    ]
    (map renderError (state ^. (_marloweState <<< _Head <<< _editorErrors)))
  where
  renderError error = pre [ class_ (ClassName "error-content") ] [ text error.text ]

spanText :: forall p. String -> HTML p HAction
spanText s = span [ class_ $ ClassName "pr-1" ] [ text s ]

hasHistory :: FrontendState -> Boolean
hasHistory state = NEL.length (view _marloweState state) > 1

-- TODO: Need to make these errors nice explanations - function in smeantics utils
printTransError :: forall p. TransactionError -> Array (HTML p HAction)
printTransError error = [ ul_ [ li_ [ text (show error) ] ] ]

transactionErrors :: forall p. Maybe TransactionError -> Array (HTML p HAction)
transactionErrors Nothing = []

transactionErrors (Just error) =
  [ div
      [ classes
          [ ClassName "invalid-transaction"
          , ClassName "input-composer"
          ]
      ]
      ( [ h2 [] [ text "The transaction is invalid:" ] ]
          <> printTransError error
      )
  ]

displayTransactionList :: forall p. String -> HTML p HAction
displayTransactionList transactionList = case runParser transactionInputList transactionList of
  Right pTL ->
    ol_
      ( do
          ( TransactionInput
              { interval: SlotInterval (Slot from) (Slot to)
            , inputs: inputList
            }
          ) <-
            ((toUnfoldable pTL) :: Array TransactionInput)
          pure
            ( li_
                [ span_
                    [ b_ [ text "Transaction" ]
                    , text " with slot interval "
                    , b_ [ text $ (show from <> " to " <> show to) ]
                    , if List.null inputList then
                        text " and no inputs (empty transaction)."
                      else
                        text " and inputs:"
                    ]
                , if List.null inputList then
                    text ""
                  else
                    displayInputList inputList
                ]
            )
      )
  Left _ -> code_ [ text transactionList ]

displayInputList :: forall p. List Input -> HTML p HAction
displayInputList inputList =
  ol_
    ( do
        input <- (toUnfoldable inputList)
        pure (li_ (displayInput input))
    )

displayInput :: forall p. Input -> Array (HTML p HAction)
displayInput (IDeposit (AccountId accNum owner) party tok money) =
  [ b_ [ text "IDeposit" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " deposits "
  , b_ [ text $ show money ]
  , text " units of "
  , b_ [ text $ show tok ]
  , text " into account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text "."
  ]

displayInput (IChoice (ChoiceId choiceId party) chosenNum) =
  [ b_ [ text "IChoice" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " chooses number "
  , b_ [ text $ show chosenNum ]
  , text " for choice "
  , b_ [ text $ show choiceId ]
  , text "."
  ]

displayInput (INotify) =
  [ b_ [ text "INotify" ]
  , text " - The contract is notified that an observation became "
  , b_ [ text "True" ]
  ]

displayWarningList :: forall p. String -> HTML p HAction
displayWarningList transactionWarnings = case runParser transactionWarningList transactionWarnings of
  Right pWL ->
    ol_
      ( do
          warning <- ((toUnfoldable pWL) :: Array TransactionWarning)
          pure (li_ (displayWarning warning))
      )
  Left _ -> code_ [ text transactionWarnings ]

displayWarnings :: forall p. Array TransactionWarning -> HTML p HAction
displayWarnings [] = text mempty

displayWarnings warnings =
  div
    [ classes
        [ ClassName "invalid-transaction"
        , ClassName "input-composer"
        ]
    ]
    [ h2 [] [ text "Warnings" ]
    , ol
        []
        $ foldMap (\warning -> [ li_ (displayWarning warning) ]) warnings
    ]

displayWarning :: forall p. TransactionWarning -> Array (HTML p HAction)
displayWarning (TransactionNonPositiveDeposit party (AccountId accNum owner) tok amount) =
  [ b_ [ text "TransactionNonPositiveDeposit" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " is asked to deposit "
  , b_ [ text $ show amount ]
  , text " units of "
  , b_ [ text $ show tok ]
  , text " into account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text "."
  ]

displayWarning (TransactionNonPositivePay (AccountId accNum owner) payee tok amount) =
  [ b_ [ text "TransactionNonPositivePay" ]
  , text " - The contract is suppoused to make a payment of "
  , b_ [ text $ show amount ]
  , text " units of "
  , b_ [ text $ show tok ]
  , text " from account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text " to "
  , b_
      [ text case payee of
          (Account (AccountId accNum2 owner2)) -> ("account " <> (show accNum2) <> " of " <> (show owner2))
          (Party dest) -> ("party " <> (show dest))
      ]
  , text "."
  ]

displayWarning (TransactionPartialPay (AccountId accNum owner) payee tok amount expected) =
  [ b_ [ text "TransactionPartialPay" ]
  , text " - The contract is suppoused to make a payment of "
  , b_ [ text $ show expected ]
  , text " units of "
  , b_ [ text $ show tok ]
  , text " from account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text " to "
  , b_
      [ text case payee of
          (Account (AccountId accNum2 owner2)) -> ("account " <> (show accNum2) <> " of " <> (show owner2))
          (Party dest) -> ("party " <> (show dest))
      ]
  , text " but there is only "
  , b_ [ text $ show amount ]
  , text "."
  ]

displayWarning (TransactionShadowing valId oldVal newVal) =
  [ b_ [ text "TransactionShadowing" ]
  , text " - The contract defined the value with id "
  , b_ [ text (show valId) ]
  , text " before, it was assigned the value "
  , b_ [ text (show oldVal) ]
  , text " and now it is being assigned the value "
  , b_ [ text (show newVal) ]
  , text "."
  ]

analysisResultPane :: forall p. FrontendState -> HTML p HAction
analysisResultPane state =
  let
    result = state ^. _analysisState
  in
    case result of
      NotAsked ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ text "Press the button below to analyse the contract for runtime warnings."
          ]
      Success (R.Valid) ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Analysis Result: Pass" ]
          , text "Static analysis could not find any execution that results in any warning."
          ]
      Success (R.CounterExample { initialSlot, transactionList, transactionWarning }) ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Analysis Result: Fail" ]
          , text "Static analysis found the following counterexample:"
          , ul_
              [ li_
                  [ spanText "Initial slot: "
                  , b_ [ spanText (show initialSlot) ]
                  ]
              , li_
                  [ spanText "Offending transaction list: "
                  , displayTransactionList transactionList
                  ]
              , li_
                  [ spanText "Warnings issued: "
                  , displayWarningList transactionWarning
                  ]
              ]
          ]
      Success (R.Error str) ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Error during analysis" ]
          , text "Analysis failed for the following reason:"
          , ul_
              [ li_
                  [ b_ [ spanText str ]
                  ]
              ]
          ]
      Failure failure ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Error during analysis" ]
          , text "Analysis failed for the following reason:"
          , ul_
              [ li_
                  [ b_ [ spanText failure ]
                  ]
              ]
          ]
      _ -> text "Analysing..."

authButton :: forall p. FrontendState -> HTML p HAction
authButton state =
  let
    authStatus = state ^. (_authStatus <<< to (map (view authStatusAuthRole)))
  in
    case authStatus of
      Failure _ ->
        button
          [ idPublishGist
          , classes []
          ]
          [ text "Failure" ]
      Success Anonymous ->
        a
          [ idPublishGist
          , classes [ClassName "auth-button"]
          , href "/api/oauth/github"
          ]
          [ text "Log In"
          ]
      Success GithubUser -> gist state
      Loading ->
        button
          [ idPublishGist
          , classes []
          , disabled true
          ]
          [ icon Spinner ]
      NotAsked ->
        button
          [ idPublishGist
          , classes []
          , disabled true
          ]
          [ icon Spinner ]

loadGistButton :: forall p. Either String (RemoteData AjaxError Gist) -> HTML p HAction
loadGistButton (Left e) =
  svg [ clazz (ClassName "error-icon"), SVG.width (Px 20), height (Px 20), viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
    [ path [ fill (Hex "#ff0000"), d "M13,13H11V7H13M12,17.3A1.3,1.3 0 0,1 10.7,16A1.3,1.3 0 0,1 12,14.7A1.3,1.3 0 0,1 13.3,16A1.3,1.3 0 0,1 12,17.3M15.73,3H8.27L3,8.27V15.73L8.27,21H15.73L21,15.73V8.27L15.73,3Z" ] [] ]

loadGistButton (Right (Failure _)) =
  svg [ clazz (ClassName "error-icon"), SVG.width (Px 20), height (Px 20), viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
    [ path [ fill (Hex "#ff0000"), d "M13,13H11V7H13M12,17.3A1.3,1.3 0 0,1 10.7,16A1.3,1.3 0 0,1 12,14.7A1.3,1.3 0 0,1 13.3,16A1.3,1.3 0 0,1 12,17.3M15.73,3H8.27L3,8.27V15.73L8.27,21H15.73L21,15.73V8.27L15.73,3Z" ] [] ]

loadGistButton (Right (Success _)) =
  svg [ clazz (ClassName "arrow-down"), SVG.width (Px 20), height (Px 20), viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
    [ path [ fill (Hex "#832dc4"), d "M19.92,12.08L12,20L4.08,12.08L5.5,10.67L11,16.17V2H13V16.17L18.5,10.66L19.92,12.08M12,20H2V22H22V20H12Z" ] [] ]

loadGistButton (Right Loading) =
  svg [ clazz (ClassName "spinner"), SVG.width (Px 65), height (Px 65), viewBox (Box { x: 0, y: 0, width: 66, height: 66 }) ]
    [ circle [ clazz (ClassName "path"), fill SVG.None, strokeWidth 6, strokeLinecap Round, cx (Length 33.0), cy (Length 33.0), r (Length 30.0) ] [] ]

loadGistButton (Right NotAsked) =
  svg [ clazz (ClassName "arrow-down"), SVG.width (Px 20), height (Px 20), viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
    [ path [ fill (Hex "#832dc4"), d "M19.92,12.08L12,20L4.08,12.08L5.5,10.67L11,16.17V2H13V16.17L18.5,10.66L19.92,12.08M12,20H2V22H22V20H12Z" ] [] ]

gistInput :: forall p. Either String (RemoteData AjaxError Gist) -> HTML p HAction
gistInput (Left _) = input [ HTML.type_ InputText, classes [ ClassName "form-control", ClassName "py-0", ClassName "error" ], HTML.id_ "github-input", placeholder "Gist ID", onValueInput $ Just <<< GistAction <<< SetGistUrl ]

gistInput (Right (Failure _)) = input [ HTML.type_ InputText, classes [ ClassName "form-control", ClassName "py-0", ClassName "error" ], HTML.id_ "github-input", placeholder "Gist ID", onValueInput $ Just <<< GistAction <<< SetGistUrl ]

gistInput _ = input [ HTML.type_ InputText, classes [ ClassName "form-control", ClassName "py-0" ], HTML.id_ "github-input", placeholder "Gist ID", onValueInput $ Just <<< GistAction <<< SetGistUrl ]
