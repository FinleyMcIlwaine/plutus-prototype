module Classes where

import Prelude
import Data.Lens (to, (^.))
import Halogen (ClassName(..))
import Types (FrontendState, View, _showBottomPanel, _view)

foreign import closeDrawerIcon :: String

foreign import githubIcon :: String

foreign import downloadIcon :: String

foreign import blocklyIcon :: String

foreign import infoIcon :: String

foreign import readMoreIconWhite :: String

foreign import iohkIcon :: String

active :: ClassName
active = ClassName "active"

noMargins :: ClassName
noMargins = ClassName "no-margins"

aHorizontal :: ClassName
aHorizontal = ClassName "a-horizontal"

spaceLeft :: ClassName
spaceLeft = ClassName "space-left"

uppercase :: ClassName
uppercase = ClassName "uppercase"

tabLink :: ClassName
tabLink = ClassName "tab-link"

aCenter :: ClassName
aCenter = ClassName "a-center"

flexCol :: ClassName
flexCol = ClassName "flex-col"

tabIcon :: ClassName
tabIcon = ClassName "tab-icon"

panelContent :: ClassName
panelContent = ClassName "panel-content"

panelHeader :: ClassName
panelHeader = ClassName "panel-header"

panelSubHeader :: ClassName
panelSubHeader = ClassName "panel-sub-header"

panelSubHeaderMain :: ClassName
panelSubHeaderMain = ClassName "panel-sub-header-main"

panelSubHeaderSide :: ClassName
panelSubHeaderSide = ClassName "panel-sub-header-side"

panelHeaderMain :: ClassName
panelHeaderMain = ClassName "panel-header-main"

panelHeaderSide :: ClassName
panelHeaderSide = ClassName "panel-header-side"

accentBorderBottom :: ClassName
accentBorderBottom = ClassName "accent-border-bottom"

jFlexStart :: ClassName
jFlexStart = ClassName "j-flex-start"

smallBtn :: ClassName
smallBtn = ClassName "small-btn"

plusBtn :: ClassName
plusBtn = ClassName "plus-btn"

minusBtn :: ClassName
minusBtn = ClassName "minus-btn"

btnSecondary :: ClassName
btnSecondary = ClassName "btn-secondary"

textSecondaryColor :: ClassName
textSecondaryColor = ClassName "text-secondary-color"

bold :: ClassName
bold = ClassName "bold"

activeTextPrimary :: ClassName
activeTextPrimary = ClassName "active-text-primary"

mAlignCenter :: ClassName
mAlignCenter = ClassName "m-align-center"

tAlignCenter :: ClassName
tAlignCenter = ClassName "t-align-center"

flex :: ClassName
flex = ClassName "flex"

flexFour :: ClassName
flexFour = ClassName "flex-four"

flexTen :: ClassName
flexTen = ClassName "flex-ten"

isActiveTab :: FrontendState -> View -> Array ClassName
isActiveTab state activeView = if state ^. _view <<< (to (eq activeView)) then [ active ] else []

isActiveDemo :: FrontendState -> Array ClassName
isActiveDemo state = if true then [ ClassName "active-text" ] else []

rTable :: ClassName
rTable = ClassName "Rtable"

rTable6cols :: ClassName
rTable6cols = ClassName "Rtable--6cols"

rTableCell :: ClassName
rTableCell = ClassName "Rtable-cell"

rTableCellHeader :: ClassName
rTableCellHeader = ClassName "Rtable-cell-header"

first :: ClassName
first = ClassName "first"

header :: ClassName
header = ClassName "header"

rTableEmptyRow :: ClassName
rTableEmptyRow = ClassName "RTable-empty-row"

stateLabel :: ClassName
stateLabel = ClassName "state-label"

pointer :: ClassName
pointer = ClassName "pointer"

analysisPanel :: FrontendState -> Array ClassName
analysisPanel state = if state ^. _showBottomPanel then [ ClassName "analysis-panel" ] else [ ClassName "analysis-panel", ClassName "collapse" ]

codeEditor :: FrontendState -> Array ClassName
codeEditor state = if state ^. _showBottomPanel then [ ClassName "code-editor" ] else [ ClassName "code-editor", ClassName "expanded" ]

footerPanelBg :: FrontendState -> Array ClassName
footerPanelBg state = if state ^. _showBottomPanel then [ ClassName "footer-panel-bg", ClassName "expanded" ] else [ ClassName "footer-panel-bg" ]

-- FIXME: get correct piece of state
githubDisplay :: FrontendState -> Array ClassName
githubDisplay state = if state ^. _showBottomPanel then [ ClassName "hover" ] else []
