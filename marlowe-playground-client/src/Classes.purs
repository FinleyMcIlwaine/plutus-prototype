module Classes where

import Halogen (ClassName(..))
import Types (FrontendState)

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
panelSubHeaderMain = ClassName "panel-sub-header-side"

panelSubHeaderSide :: ClassName
panelSubHeaderSide = ClassName "panel-sub-header-main"

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

isActive :: FrontendState -> Array ClassName
isActive state = if true then [ active ] else []

isActiveDemo :: FrontendState -> Array ClassName
isActiveDemo state = if true then [ ClassName "active-text" ] else []
