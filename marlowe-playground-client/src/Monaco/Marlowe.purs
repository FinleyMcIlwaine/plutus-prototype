module Monaco.Marlowe where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Monaco (LanguageExtensionPoint(..), MonarchLanguage(..), MonarchLanguageBracket(..), default, simpleRule, simpleRuleCases, simpleRuleWithAction, simpleRuleWithLog)

languageExtensionPoint :: LanguageExtensionPoint
languageExtensionPoint = LanguageExtensionPoint { id: "marlowe" }

_id :: Lens' LanguageExtensionPoint String
_id = _Newtype <<< prop (SProxy :: SProxy "id")

monarchLanguage :: MonarchLanguage
monarchLanguage =
  let
    tokenizer =
      Object.fromFoldable
        [ "root"
            /\ [ simpleRuleCases (unsafeRegex "[A-Z][a-z$]*" noFlags) [ Tuple "@keywords" "keyword" ]
              , simpleRule (unsafeRegex "[ \\t\\r\\n]+" noFlags) "white"
              -- TODO: monaco version has /"([^"\\]|\\.)*$/ not sure exactly what this is
              , simpleRuleWithLog (unsafeRegex "\"*$" noFlags) "string.invalid" "string.invalid"
              , simpleRuleWithAction (unsafeRegex "\"" noFlags) "string.quote" "@string"
              , simpleRule (unsafeRegex "[()]" noFlags) "@brackets"
              ]
        , "string"
            /\ [ simpleRule (unsafeRegex """[^\\"]+""" noFlags) "string"
              , simpleRuleWithAction (unsafeRegex "\"" noFlags) "string" "@pop"
              ]
        ]

    brackets = Just [ MonarchLanguageBracket { open: "(", close: ")", token: "delimiter.parenthesis" } ]

    keywords = Just [ "Close", "If" ]

    lang r = r { tokenizer = tokenizer, brackets = brackets, defaultToken = Just "invalid", keywords = keywords }
  in
    Newtype.over MonarchLanguage lang default
