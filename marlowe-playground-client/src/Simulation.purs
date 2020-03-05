module Simulation where

import API (RunResult(RunResult))
import Bootstrap (btn, btnInfo, btnPrimary, btnSecondary, btnSmall, card, cardBody_, card_, col3_, col6, col9, col_, dropdownToggle, empty, listGroupItem_, listGroup_, row_)
import Bootstrap.Extra (ariaExpanded, ariaHasPopup, ariaLabelledBy, dataToggle)
import Classes (aHorizontal, accentBorderBottom, activeTextPrimary, blocklyIcon, bold, closeDrawerIcon, downloadIcon, first, githubIcon, infoIcon, isActiveDemo, isActiveTab, jFlexStart, minusBtn, noMargins, panelHeader, panelHeaderMain, panelHeaderSide, panelSubHeader, panelSubHeaderMain, panelSubHeaderSide, plusBtn, rTable, rTable6cols, rTableCell, rTableEmptyRow, smallBtn, spaceLeft, textSecondaryColor, uppercase)
import Classes as Classes
import Control.Alternative (map)
import Data.Array (catMaybes, concatMap, fromFoldable, head, length, sortBy)
import Data.Array as Array
import Data.BigInteger (BigInteger, fromString, fromInt)
import Data.Either (Either(..))
import Data.Eq (eq, (/=), (==))
import Data.Foldable (foldMap, intercalate)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HeytingAlgebra ((&&), (||))
import Data.Lens (to, view, (^.))
import Data.List (List, toUnfoldable, null)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (uncurry5, (/\), type (/\))
import Debug.Trace (trace)
import Editor (initEditor) as Editor
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML (ClassName(..), ComponentHTML, HTML, PropName(..), a, a_, article, aside, b_, br_, button, code_, col, colgroup, div, div_, em_, h2, h3_, h4, h6, h6_, img, input, li, li_, ol, ol_, p_, pre, pre_, section, slot, small, small_, span, span_, strong_, table_, tbody_, td, td_, text, th, th_, thead_, tr, ul, ul_)
import Halogen.HTML.Events (onClick, onDragOver, onDrop, onValueChange)
import Halogen.HTML.Properties (ButtonType(..), InputType(InputNumber), alt, class_, classes, enabled, id_, placeholder, prop, src, type_, value)
import Halogen.HTML.Properties.ARIA (role)
import Help (toHTML)
import Halogen.Monaco (monacoComponent)
import Marlowe.Holes (Holes(..), MarloweHole(..), MarloweType(..), getMarloweConstructors)
import Marlowe.Parser (currencySymbol, transactionInputList, transactionWarningList)
import Marlowe.Semantics (AccountId(..), Assets(..), Bound(..), ChoiceId(..), ChosenNum, CurrencySymbol, Input(..), Party, Payee(..), Payment(..), PubKey, Slot(..), SlotInterval(..), Token(..), TokenName, TransactionError, TransactionInput(..), TransactionWarning(..), ValueId(..), _accounts, _boundValues, _choices, inBounds, maxTime)
import Marlowe.Symbolic.Types.Response as R
import Network.RemoteData (RemoteData(..), isLoading)
import Prelude (class Show, Unit, bind, compare, const, flip, identity, mempty, not, pure, show, unit, zero, ($), (+), (<$>), (<<<), (<>), (>))
import StaticData as StaticData
import Text.Parsing.StringParser (runParser)
import Types (ActionInput(..), ActionInputId, ChildSlots, FrontendState, HAction(..), HelpContext(..), MarloweError(..), MarloweState, SimulationBottomPanelView(..), View(..), _Head, _analysisState, _contract, _editorErrors, _editorPreferences, _editorWarnings, _helpContext, _holes, _marloweCompileResult, _marloweEditorSlot, _marloweState, _payments, _pendingInputs, _possibleActions, _selectedHole, _simulationBottomPanelView, _slot, _state, _transactionError, _transactionWarnings)
import Types (ActionInput(..), ActionInputId, ChildSlots, FrontendState, HAction(..), MarloweError(..), MarloweState, _Head, _analysisState, _contract, _editorErrors, _editorPreferences, _holes, _marloweCompileResult, _marloweEditorSlot, _marloweState, _monacoSlot, _payments, _pendingInputs, _possibleActions, _selectedHole, _slot, _state, _transactionError, _transactionWarnings)
import Types (ActionInput(..), ActionInputId, ChildSlots, FrontendState, HAction(..), MarloweError(..), MarloweState, _Head, _analysisState, _contract, _editorErrors, _holes, _marloweCompileResult, _marloweState, _monacoSlot, _payments, _pendingInputs, _possibleActions, _selectedHole, _slot, _state, _transactionError, _transactionWarnings)
import Types (ActionInput(..), ActionInputId, ChildSlots, FrontendState, HAction(..), MarloweError(..), MarloweState, _Head, _analysisState, _contract, _editorErrors, _holes, _marloweCompileResult, _marloweEditorSlot, _marloweState, _payments, _pendingInputs, _possibleActions, _selectedHole, _slot, _state, _transactionError, _transactionWarnings)

paneHeader :: forall p. String -> HTML p HAction
paneHeader s = h2 [ class_ $ ClassName "pane-header" ] [ text s ]

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
          [ a []
              [ img [ class_ (ClassName "drawer-icon"), src closeDrawerIcon, alt "close drawer icon" ]
              ]
          , div [ class_ aHorizontal ]
              [ a [] [ img [ class_ (ClassName "github-icon"), src githubIcon, alt "github icon" ] ]
              , button [ class_ spaceLeft ] [ text "Save to github" ]
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
      [ div [ class_ (ClassName "code-editor") ]
          [ marloweEditor state ]
      , sidebar state
      ]
  ]
  where
  demoScriptLink key = li [ classes (isActiveDemo state) ] [ a [ onClick $ const $ Just $ LoadMarloweScript key ] [ text key ] ]

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
  li [ classes [ ClassName "choice-a", aHorizontal ] ]
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
        , enabled isEnabled
        , onClick $ const $ Just
            $ AddInput person (IChoice (ChoiceId choiceName choiceOwner) chosenNum) bounds
        ]
        [ text "+" ]
    , p_
        [ spanText "Choice "
        , b_ [ spanText (show choiceName) ]
        , spanText ": Choose value "
        , marloweActionInput isEnabled (SetChoice choiceId) chosenNum
        ]
    , input []
    ]

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
            [ li_
                [ a
                    [ onClick
                        $ if hasHistory state then
                            Just <<< const Undo
                          else
                            const Nothing
                    ]
                    [ text "Undo" ]
                ]
            , li_
                [ a
                    [ onClick
                        $ if hasHistory state then
                            Just <<< const ResetSimulator
                          else
                            const Nothing
                    ]
                    [ text "Reset" ]
                ]
            , li [ classes [ activeTextPrimary, bold ] ]
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
                    , enabled $ trace (isContractValid state) \_ -> isContractValid state
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

bottomPanel :: forall p. FrontendState -> Array (HTML p HAction)
bottomPanel state =
  [ div [ classes ([ ClassName "footer-panel-bg" ] <> isActiveTab state Simulation) ]
      [ section [ classes [ ClassName "panel-header", aHorizontal ] ]
          [ div [ classes [ ClassName "panel-sub-header-main", aHorizontal, accentBorderBottom ] ]
              [ ul [ classes [ ClassName "demo-list", aHorizontal ] ]
                  [ li
                      [ classes ([] <> isActive CurrentStateView)
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
                  ]
              ]
          ]
      , panelContents state (state ^. _simulationBottomPanelView)
      ]
  ]
  where
  isActive view = if state ^. _simulationBottomPanelView <<< (to (eq view)) then [ ClassName "active-text" ] else []

  warnings = state ^. (_marloweState <<< _Head <<< _editorWarnings)

  errors = state ^. (_marloweState <<< _Head <<< _editorErrors)

  contractMaxTime Nothing = "Closed"

  contractMaxTime (Just contract) = let t = maxTime contract in if t == zero then "Closed" else show t

panelContents :: forall p. FrontendState -> SimulationBottomPanelView -> HTML p HAction
panelContents state CurrentStateView =
  div [ classes [ rTable, rTable6cols ] ]
    ( tableRow "Accounts" "No accounts have been used" "Account ID" "Participant" "Currency Symbol" "Token Name" "Money"
        accountsData
        <> tableRow "Choices" "No Choices have been made" "Choice ID" "Participant" "Chosen Value" "" ""
            choicesData
        <> tableRow "Payments" "No payments have been recorded" "Party" "Currency Symbol" "Token Name" "Money" "" paymentsData
        <> tableRow "Let Bindings" "No values have been bound" "Identifier" "Value" "" "" "" bindingsData
    )
  where
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

panelContents state StaticAnalysisView =
  section
    [ classes [ ClassName "panel-sub-header", aHorizontal ]
    ]
    [ text "sa" ]

panelContents state MarloweWarningsView =
  section
    [ classes [ ClassName "panel-sub-header", aHorizontal ]
    ]
    (map renderWarning (state ^. (_marloweState <<< _Head <<< _editorWarnings)))
  where
  renderWarning warning = pre [ class_ (ClassName "warning-content") ] [ text warning.text ]

panelContents state MarloweErrorsView =
  section
    [ classes [ ClassName "panel-sub-header", aHorizontal ]
    ]
    (map renderError (state ^. (_marloweState <<< _Head <<< _editorErrors)))
  where
  renderError error = pre [ class_ (ClassName "error-content") ] [ text error.text ]

------------------------------------------------------------ Old Design -------------------------------------------------------
paneHeader :: forall p. String -> HTML p HAction
paneHeader s = h2 [ class_ $ ClassName "pane-header" ] [ text s ]

simulationPaneOld ::
  forall m.
  MonadAff m =>
  FrontendState ->
  ComponentHTML HAction ChildSlots m
simulationPaneOld state =
  div_
    ( Array.concat
        [ [ row_
              [ inputComposerPane state
              , transactionComposerPane state
              ]
          , row_ [ displayWarnings warnings ]
          , row_ [ col_ (state ^. (_marloweState <<< _Head <<< _transactionError <<< to transactionErrors)) ]
          , stateTitle state
          , row_ [ statePane state ]
          ]
        , [ div
              [ classes
                  [ ClassName "demos"
                  , ClassName "d-flex"
                  , ClassName "flex-row"
                  , ClassName "align-items-center"
                  , ClassName "justify-content-between"
                  , ClassName "mt-5"
                  , ClassName "mb-3"
                  ]
              ]
              [ paneHeader "Marlowe Contract", codeToBlocklyButton state, demoScriptsPane ]
          , div
              [ onDragOver $ Just <<< MarloweHandleDragEvent
              , onDrop $ Just <<< MarloweHandleDropEvent
              ]
              [ row_
                  [ div [ class_ col9 ] [ slot _marloweEditorSlot unit monacoComponent unit (Just <<< MarloweHandleEditorMessage) ]
                  , holesPane (view _selectedHole state) (view (_marloweState <<< _Head <<< _holes) $ state)
                  ]
              ]
          , br_
          , errorList
          , analysisPane state
          ]
        ]
    )
  where
  errorList = case view _marloweCompileResult state of
    Left errors -> listGroup_ (listGroupItem_ <<< pure <<< compilationErrorPane <$> errors)
    _ -> empty

  warnings = view (_marloweState <<< _Head <<< _transactionWarnings) state

holesPane :: forall p. Maybe String -> Holes -> HTML p HAction
holesPane selectedHole (Holes holes) =
  let
    kvs = Map.toUnfoldable holes

    sortHoles = compare `on` (head <<< Set.toUnfoldable <<< snd)

    ordered = sortBy sortHoles kvs

    holesGroup = map (\(Tuple k v) -> displayHole selectedHole k v) ordered
  in
    col3_
      [ div
          [ class_ $ ClassName "btn-group-vertical"
          , role "group"
          ]
          holesGroup
      ]

displayHole :: forall p. Maybe String -> String -> Set MarloweHole -> HTML p HAction
displayHole selectedHole name holes =
  div [ classes ([ ClassName "btn-group" ] <> showClass) ]
    [ button
        [ classes [ btn, btnSecondary, dropdownToggle, ClassName "button-box" ]
        , id_ ("hole-btn-" <> name)
        , type_ ButtonButton
        , dataToggle "dropdown"
        , ariaHasPopup true
        , ariaExpanded expanded
        , onClick $ const $ Just $ SelectHole selectHole
        ]
        [ text name ]
    , div
        [ classes ([ ClassName "dropdown-menu" ] <> showClass)
        , ariaLabelledBy ("hole-btn-" <> name)
        ]
        (holeDropdowns (Set.toUnfoldable holes))
    ]
  where
  expanded = selectedHole == Just name

  showClass = if selectedHole == Just name then [ ClassName "show" ] else []

  selectHole = if selectedHole == Just name then Nothing else Just name

holeDropdowns :: forall p. Array MarloweHole -> Array (HTML p HAction)
holeDropdowns holes = case Array.uncons holes of
  Nothing -> mempty
  Just { head: (MarloweHole { marloweType: BigIntegerType, row, column }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition row column
        ]
        [ text "Replace the hole with an integer" ]
    ]
  Just { head: (MarloweHole { marloweType: StringType, row, column }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition row column
        ]
        [ text "Replace the hole with a string" ]
    ]
  Just { head: (MarloweHole { marloweType: ValueIdType, row, column }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition row column
        ]
        [ text "Replace the hole with a string" ]
    ]
  Just { head: (MarloweHole { marloweType: SlotType, row, column }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition row column
        ]
        [ text "Replace the hole with an integer" ]
    ]
  Just { head: hole@(MarloweHole { marloweType }) } ->
    map
      ( \constructor ->
          a
            [ class_ $ ClassName "dropdown-item"
            , onClick $ const $ Just $ InsertHole constructor hole holes
            ]
            [ text constructor ]
      )
      (fromFoldable $ Map.keys $ getMarloweConstructors marloweType)

demoScriptsPane :: forall p. HTML p HAction
demoScriptsPane =
  div_
    ( Array.cons
        ( strong_
            [ text "Demos: "
            ]
        )
        (demoScriptButton <$> Array.fromFoldable (Map.keys StaticData.marloweContracts))
    )

demoScriptButton :: forall p. String -> HTML p HAction
demoScriptButton key =
  button
    [ classes [ btn, btnInfo, btnSmall ]
    , onClick $ const $ Just $ LoadMarloweScript key
    ]
    [ text key ]

codeToBlocklyButton :: forall p. FrontendState -> HTML p HAction
codeToBlocklyButton state =
  button
    [ classes [ btn, btnInfo, btnSmall ]
    , onClick $ const $ Just $ SetBlocklyCode
    , enabled (isContractValid state)
    ]
    [ text "Code to Blockly" ]

compilationResultPane :: forall p. RunResult -> HTML p HAction
compilationResultPane (RunResult stdout) = div_ [ code_ [ pre_ [ text stdout ] ] ]

compilationErrorPane :: forall p. MarloweError -> HTML p HAction
compilationErrorPane (MarloweError error) = div_ [ text error ]

inputComposerPane :: forall p. FrontendState -> HTML p HAction
inputComposerPane state =
  div
    [ classes
        [ col6
        , ClassName "input-composer"
        ]
    ]
    [ paneHeader "Input Composer"
    , div
        [ class_ $ ClassName "wallet"
        ]
        [ card_
            [ cardBody_ (inputComposerOld isEnabled (view (_marloweState <<< _Head <<< _possibleActions) state))
            ]
        ]
    ]
  where
  isEnabled = isContractValid state

onEmpty :: forall a. Array a -> Array a -> Array a
onEmpty alt [] = alt

onEmpty _ arr = arr

inputComposerOld :: forall p. Boolean -> Map (Maybe PubKey) (Map ActionInputId ActionInput) -> Array (HTML p HAction)
inputComposerOld isEnabled actionInputs =
  if (Map.isEmpty actionInputs) then
    [ text "No valid inputs can be added to the transaction" ]
  else
    (actionsForPeople actionInputs)
  where
  kvs :: forall k v. Map k v -> Array (Tuple k v)
  kvs = Map.toUnfoldable

  vs :: forall k v. Map k v -> Array v
  vs m = map snd (kvs m)

  lastKey :: Maybe (Maybe PubKey)
  lastKey = map (\x -> x.key) (Map.findMax actionInputs)

  actionsForPeople :: forall q. Map (Maybe PubKey) (Map ActionInputId ActionInput) -> Array (HTML q HAction)
  actionsForPeople m = foldMap (\(Tuple k v) -> inputComposerPerson isEnabled k (vs v) (Just k == lastKey)) (kvs m)

inputComposerPerson ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  Array ActionInput ->
  Boolean ->
  Array (HTML p HAction)
inputComposerPerson isEnabled maybePerson actionInputs isLast =
  [ h3_
      [ text
          ( case maybePerson of
              Just person -> ("Participant " <> show person)
              Nothing -> ("Anyone")
          )
      ]
  ]
    <> [ div [ class_ $ ClassName (if isLast then "state-last-row" else "state-row") ]
          (catMaybes (mapWithIndex inputForAction actionInputs))
      ]
  where
  inputForAction :: Int -> ActionInput -> Maybe (HTML p HAction)
  inputForAction index (DepositInput accountId party token value) = Just $ inputDepositOld isEnabled maybePerson index accountId party token value

  inputForAction index (ChoiceInput choiceId bounds chosenNum) = Just $ inputChoiceOld isEnabled maybePerson index choiceId chosenNum bounds

  inputForAction index NotifyInput = Just $ inputNotifyOld isEnabled maybePerson index

inputDepositOld ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  Int ->
  AccountId ->
  Party ->
  Token ->
  BigInteger ->
  HTML p HAction
inputDepositOld isEnabled person index accountId party token value =
  div_
    $ [ button
          [ class_ $ ClassName "composer-add-button"
          , enabled isEnabled
          , onClick $ const $ Just
              $ AddInput person (IDeposit accountId party token value) []
          ]
          [ text "+"
          ]
      ]
    <> (renderDeposit accountId party token value)

inputChoiceOld :: forall p. Boolean -> Maybe PubKey -> Int -> ChoiceId -> ChosenNum -> Array Bound -> HTML p HAction
inputChoiceOld isEnabled person index choiceId@(ChoiceId choiceName choiceOwner) chosenNum bounds =
  let
    validBounds = inBounds chosenNum bounds

    errorRow = if validBounds then [] else [ text boundsError ]
  in
    div_
      ( [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ const $ Just
                $ AddInput person (IChoice (ChoiceId choiceName choiceOwner) chosenNum) bounds
            ]
            [ text "+"
            ]
        , spanText "Choice "
        , b_ [ spanText (show choiceName) ]
        , spanText ": Choose value "
        , marloweActionInput isEnabled (SetChoice choiceId) chosenNum
        ]
          <> errorRow
      )
  where
  boundsError = "Choice must be between " <> intercalate " or " (map boundError bounds)

  boundError (Bound from to) = show from <> " and " <> show to

inputNotifyOld ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  Int ->
  HTML p HAction
inputNotifyOld isEnabled person index =
  div_
    [ button
        [ class_ $ ClassName "composer-add-button"
        , enabled isEnabled
        , onClick $ const $ Just
            $ AddInput person INotify []
        ]
        [ text "+"
        ]
    , text $ "Notify contract"
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

flexRow_ ::
  forall p.
  Array (HTML p HAction) ->
  HTML p HAction
flexRow_ html = div [ classes [ ClassName "d-flex", ClassName "flex-row" ] ] html

spanText :: forall p. String -> HTML p HAction
spanText s = span [ class_ $ ClassName "pr-1" ] [ text s ]

transactionComposerPane ::
  forall p.
  FrontendState ->
  HTML p HAction
transactionComposerPane state =
  div
    [ classes
        [ col6
        , ClassName "input-composer"
        ]
    ]
    [ paneHeader "Transaction Composer"
    , div
        [ class_ $ ClassName "wallet"
        ]
        [ div
            [ classes
                ( ( if view (_marloweState <<< _Head <<< _transactionError <<< to isJust) state then
                      (flip Array.snoc) (ClassName "invalid-transaction")
                    else
                      identity
                  )
                    [ card ]
                )
            ]
            [ cardBody_
                $ transactionInputs (view (_marloweState <<< _Head) state)
                <> transactionButtons state
            ]
        ]
    ]

transactionButtons :: FrontendState -> forall p. Array (HTML p HAction)
transactionButtons state =
  [ div
      [ classes
          [ ClassName "d-flex"
          , ClassName "flex-row"
          , ClassName "align-items-center"
          , ClassName "justify-content-start"
          , ClassName "transaction-btn-row"
          ]
      ]
      [ button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , onClick $ Just <<< const ApplyTransaction
          , enabled $ isContractValid state
          ]
          [ text "Apply Transaction" ]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , onClick $ Just <<< const NextSlot
          , enabled (isContractValid state)
          ]
          [ text "Next Block" ]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , enabled (hasHistory state)
          , onClick $ Just <<< const ResetSimulator
          ]
          [ text "Reset" ]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , enabled (hasHistory state)
          , onClick $ Just <<< const Undo
          ]
          [ text "Undo" ]
      ]
  ]

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

transactionInputs :: forall p. MarloweState -> Array (HTML p HAction)
transactionInputs state =
  [ h3_
      [ text "Input list"
      ]
  ]
    <> [ div [ class_ $ ClassName "state-row" ]
          ( onEmpty [ text "No inputs in the transaction" ]
              $ mapWithOneIndex (inputRow isEnabled) (state ^. _pendingInputs)
          )
      ]
  where
  isEnabled = state.contract /= Nothing || state.editorErrors /= []

  mapWithOneIndex f = mapWithIndex (\i a -> f (i + 1) a)

inputRow :: forall p. Boolean -> Int -> Tuple Input (Maybe PubKey) -> HTML p HAction
inputRow isEnabled idx (Tuple INotify person) =
  row_
    [ col_
        [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ const $ Just $ RemoveInput person INotify
            ]
            [ text $ "- " <> show idx <> ":"
            ]
        , text "Notification"
        ]
    ]

inputRow isEnabled idx (Tuple input@(IDeposit accountId party token money) person) =
  row_
    [ col_
        $ [ button
              [ class_ $ ClassName "composer-add-button"
              , enabled isEnabled
              , onClick $ const $ Just $ RemoveInput person input
              ]
              [ text $ "- " <> show idx <> ":"
              ]
          ]
        <> (renderDeposit accountId party token money)
    ]

inputRow isEnabled idx (Tuple input@(IChoice (ChoiceId choiceName choiceOwner) chosenNum) person) =
  row_
    [ col_
        [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ const $ Just $ RemoveInput person input
            ]
            [ text $ "- " <> show idx <> ":"
            ]
        , text "Participant "
        , b_
            [ text (show choiceOwner)
            ]
        , text " chooses the value "
        , b_
            [ text (show chosenNum)
            ]
        , text " for choice with id "
        , b_
            [ text (show choiceName)
            ]
        ]
    ]

stateTitle ::
  forall p.
  FrontendState ->
  HTML p HAction
stateTitle state =
  div
    [ classes
        [ ClassName "demos"
        , ClassName "d-flex"
        , ClassName "flex-row"
        , ClassName "align-items-center"
        , ClassName "justify-content-between"
        , ClassName "mt-3"
        , ClassName "mb-3"
        ]
    ]
    [ paneHeader "State"
    , span
        [ classes
            [ btn
            , btnSmall
            ]
        ]
        [ strong_
            [ text "Contract expiration (slot):"
            ]
        , span
            [ class_ $ ClassName "max-contract-length"
            ]
            [ view (_marloweState <<< _Head <<< _contract <<< to contractMaxTime <<< to text) state
            ]
        , strong_
            [ text "Current Block:"
            ]
        , span
            [ class_ $ ClassName "block-number"
            ]
            [ view (_marloweState <<< _Head <<< _slot <<< to show <<< to text) state
            ]
        ]
    ]
  where
  contractMaxTime Nothing = "Closed"

  contractMaxTime (Just contract) = let t = maxTime contract in if t == zero then "Closed" else show t

statePane :: forall p. FrontendState -> HTML p HAction
statePane state =
  div
    [ class_ $ ClassName "col"
    ]
    [ stateTable state
    ]

stateTable :: forall p. FrontendState -> HTML p HAction
stateTable state =
  div_
    [ card_
        [ cardBody_
            [ h3_
                [ text "Accounts"
                ]
            , div
                [ classes [ ClassName "state-row", ClassName "full-width-card-5" ] ]
                [ if (Map.size accounts == 0) then
                    text "There are no accounts in the state"
                  else
                    renderAccounts accounts
                ]
            , h3_
                [ text "Choices"
                ]
            , div
                [ classes [ ClassName "state-row", ClassName "full-width-card" ] ]
                [ if (Map.size choices == 0) then
                    text "No choices have been recorded"
                  else
                    renderChoices choices
                ]
            , h3_
                [ text "Payments"
                ]
            , div
                [ classes [ ClassName "state-row", ClassName "full-width-card-4" ] ]
                [ if (Array.length payments == 0) then
                    text "No payments have been recorded"
                  else
                    renderPayments payments
                ]
            , h3_
                [ text "Let bindings"
                ]
            , div
                [ classes [ ClassName "state-last-row", ClassName "full-width-card" ] ]
                [ if (Map.size bindings == 0) then
                    text "No values have been bound"
                  else
                    renderBindings bindings
                ]
            ]
        ]
    ]
  where
  accounts = state ^. _marloweState <<< _Head <<< _state <<< _accounts

  choices = state ^. _marloweState <<< _Head <<< _state <<< _choices

  payments = state ^. _marloweState <<< _Head <<< _payments

  bindings = state ^. _marloweState <<< _Head <<< _state <<< _boundValues

renderAccounts :: forall p. Map (Tuple AccountId Token) BigInteger -> HTML p HAction
renderAccounts accounts =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Account id"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Participant"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Currency symbol"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Token name"
                ]
            , th_
                [ text "Money"
                ]
            ]
        ]
    , tbody_ (map renderAccount accountList)
    ]
  where
  accountList = Map.toUnfoldable accounts :: Array (Tuple (Tuple AccountId Token) BigInteger)

renderAccount :: forall p. Tuple (Tuple AccountId Token) BigInteger -> HTML p HAction
renderAccount (Tuple (Tuple (AccountId accountNumber accountOwner) (Token currSym tokName)) value) =
  tr []
    [ td_
        [ text (show accountNumber)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show accountOwner)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show currSym)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show tokName)
        ]
    , td_
        [ text (show value)
        ]
    ]

renderChoices :: forall p. Map ChoiceId ChosenNum -> HTML p HAction
renderChoices choices =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Choice id"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Participant"
                ]
            , th_
                [ text "Chosen value"
                ]
            ]
        ]
    , tbody_ (map renderChoice choiceList)
    ]
  where
  choiceList = Map.toUnfoldable choices :: Array (Tuple ChoiceId ChosenNum)

renderChoice :: forall p. Tuple ChoiceId ChosenNum -> HTML p HAction
renderChoice (Tuple (ChoiceId choiceName choiceOwner) value) =
  tr []
    [ td_
        [ text (show choiceName)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show choiceOwner)
        ]
    , td_
        [ text (show value)
        ]
    ]

renderPayments :: forall p. Array Payment -> HTML p HAction
renderPayments payments =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Party"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Currency symbol"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Token name"
                ]
            , th
                [ class_ $ ClassName "left-border-column"
                ]
                [ text "Money"
                ]
            ]
        ]
    , tbody_ (flattenPayments payments)
    ]

flattenPayments :: forall p. Array Payment -> Array (HTML p HAction)
flattenPayments payments =
  concatMap
    ( \(Payment party (Assets mon)) ->
        concatMap
          ( \(Tuple curr toks) ->
              map
                ( \(Tuple tok val) ->
                    renderPayment party curr tok val
                )
                $ Map.toUnfoldable toks
          )
          $ Map.toUnfoldable mon
    )
    payments

renderPayment :: forall p. Party -> CurrencySymbol -> TokenName -> BigInteger -> HTML p HAction
renderPayment party currSym tokName money =
  tr []
    [ td_
        [ text (show party)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show currSym)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show tokName)
        ]
    , td
        [ class_ $ ClassName "left-border-column"
        ]
        [ text (show money)
        ]
    ]

renderBindings :: forall p. Map ValueId BigInteger -> HTML p HAction
renderBindings bindings =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Identifier"
                ]
            , th
                [ class_ $ ClassName "left-border-column"
                ]
                [ text "Value"
                ]
            ]
        ]
    , tbody_ (map renderBinding bindingList)
    ]
  where
  bindingList = Map.toUnfoldable bindings :: Array (Tuple ValueId BigInteger)

renderBinding :: forall p. Tuple ValueId BigInteger -> HTML p HAction
renderBinding (Tuple (ValueId valueId) value) =
  tr []
    [ td_
        [ text (show valueId)
        ]
    , td
        [ class_ $ ClassName "left-border-column"
        ]
        [ text (show value)
        ]
    ]

analysisPane :: forall p. FrontendState -> HTML p HAction
analysisPane state =
  div [ class_ $ ClassName "full-width-card" ]
    [ paneHeader "Static analysis"
    , card_
        [ cardBody_
            [ analysisResultPane state
            , button
                [ classes
                    [ btn
                    , btnPrimary
                    , ClassName "transaction-btn"
                    ]
                , onClick $ const $ Just $ AnalyseContract
                , enabled $ state ^. _analysisState <<< to (not isLoading)
                ]
                [ loading
                , text btnText
                ]
            ]
        ]
    ]
  where
  btnText = case state ^. _analysisState of
    Loading -> "  Analysing..."
    _ -> "Analyse Contract"

  loading = case state ^. _analysisState of
    Loading ->
      span
        [ classes
            [ ClassName "spinner-border"
            , ClassName "spinner-border-sm"
            ]
        , prop (PropName "role") "status"
        , prop (PropName "aria-hidden") "true"
        ]
        []
    _ -> empty

analysisResultPane :: forall p. FrontendState -> HTML p HAction
analysisResultPane state =
  let
    result = state ^. _analysisState
  in
    case result of
      NotAsked ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ text "Press the button below to analyse the contract for runtime warnings." ]
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
      _ -> empty

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
                    , if null inputList then
                        text " and no inputs (empty transaction)."
                      else
                        text " and inputs:"
                    ]
                , if null inputList then
                    empty
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
