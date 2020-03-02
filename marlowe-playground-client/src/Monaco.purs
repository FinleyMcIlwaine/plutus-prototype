module Monaco where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Regex (Regex)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Foreign (unsafeToForeign)
import Foreign.Generic (class Encode, Foreign, SumEncoding(..), defaultOptions, encode, genericEncode)
import Foreign.Object (Object)
import Foreign.Object as Object
import Web.HTML (HTMLElement)

class Default a where
  default :: a

newtype LanguageExtensionPoint
  = LanguageExtensionPoint { id :: String }

derive instance newtypeLanguageExtensionPoint :: Newtype LanguageExtensionPoint _

derive instance genericLanguageExtensionPoint :: Generic LanguageExtensionPoint _

derive newtype instance encodeLanguageExtensionPoint :: Encode LanguageExtensionPoint

newtype MonarchLanguageBracket
  = MonarchLanguageBracket { close :: String, open :: String, token :: String }

derive instance newtypeMonarchLanguageBracket :: Newtype MonarchLanguageBracket _

derive instance genericMonarchLanguageBracket :: Generic MonarchLanguageBracket _

derive newtype instance encodeMonarchLanguageBracket :: Encode MonarchLanguageBracket

data Action
  = Action { token :: String, next :: Maybe String, log :: Maybe String }
  | Cases { cases :: (Object String), log :: Maybe String }

derive instance genericAction :: Generic Action _

instance encodeAction :: Encode Action where
  encode a =
    let
      sumEncoding =
        TaggedObject
          { tagFieldName: "tag"
          , contentsFieldName: "contents"
          , constructorTagTransform: identity
          , unwrapRecords: true
          }
    in
      genericEncode (defaultOptions { sumEncoding = sumEncoding }) a

newtype LanguageRule
  = LanguageRule { regex :: Regex, action :: Action }

derive instance newtypeLanguageRule :: Newtype LanguageRule _

derive instance genericLanguageRule :: Generic LanguageRule _

instance encodeLanguageRule :: Encode LanguageRule where
  encode (LanguageRule r) = encode { regex: unsafeToForeign r.regex, action: r.action }

simpleRule :: Regex -> String -> LanguageRule
simpleRule regex token = LanguageRule { regex, action: Action { token, next: Nothing, log: Nothing } }

simpleRuleWithLog :: Regex -> String -> String -> LanguageRule
simpleRuleWithLog regex token msg = LanguageRule { regex, action: Action { token, next: Nothing, log: Just msg } }

simpleRuleWithAction :: Regex -> String -> String -> LanguageRule
simpleRuleWithAction regex token next = LanguageRule { regex, action: Action { token, next: Just next, log: Nothing } }

simpleRuleCases :: Regex -> Array (Tuple String String) -> LanguageRule
simpleRuleCases regex cases = LanguageRule { regex, action: Cases { log: Nothing, cases: (Object.fromFoldable cases) } }

simpleRuleCasesWithLog :: Regex -> String -> Array (Tuple String String) -> LanguageRule
simpleRuleCasesWithLog regex msg cases = LanguageRule { regex, action: Cases { log: Just msg, cases: (Object.fromFoldable cases) } }

newtype MonarchLanguage
  = MonarchLanguage
  { brackets :: Maybe (Array MonarchLanguageBracket)
  , defaultToken :: Maybe String
  , ignoreCase :: Maybe Boolean
  , start :: Maybe String
  , tokenPostfix :: Maybe String
  , tokenizer :: Object (Array LanguageRule)
  -- FIXME: I need to have any record key I want here, to be extensible
  , keywords :: Maybe (Array String)
  }

derive instance newtypeMonarchLanguage :: Newtype MonarchLanguage _

derive instance genericMonarchLanguage :: Generic MonarchLanguage _

derive newtype instance encodeMonarchLanguage :: Encode MonarchLanguage

instance defaultMonarchLanguage :: Default MonarchLanguage where
  default =
    MonarchLanguage
      { brackets: Nothing
      , defaultToken: Nothing
      , ignoreCase: Nothing
      , start: Nothing
      , tokenPostfix: Nothing
      , tokenizer: mempty
      , keywords: Nothing
      }

foreign import data Monaco :: Type

foreign import data ITextModel :: Type

foreign import data TokensProvider :: Type

foreign import getMonaco :: Effect Monaco

foreign import create_ :: EffectFn3 Monaco HTMLElement String Unit

foreign import registerLanguage_ :: EffectFn2 Monaco Foreign Unit

foreign import setMonarchTokensProvider_ :: EffectFn3 Monaco String Foreign Unit

foreign import getModel_ :: EffectFn1 Monaco ITextModel

foreign import marloweTokensProvider :: TokensProvider

foreign import setTokensProvider_ :: EffectFn3 Monaco String TokensProvider Unit

create :: Monaco -> HTMLElement -> String -> Effect Unit
create = runEffectFn3 create_

registerLanguage :: Monaco -> LanguageExtensionPoint -> Effect Unit
registerLanguage monaco language =
  let
    languageF = encode language
  in
    runEffectFn2 registerLanguage_ monaco languageF

setMonarchTokensProvider :: Monaco -> String -> MonarchLanguage -> Effect Unit
setMonarchTokensProvider monaco languageId languageDef =
  let
    languageDefF = encode languageDef
  in
    runEffectFn3 setMonarchTokensProvider_ monaco languageId languageDefF

setMarloweTokensProvider :: Monaco -> String -> Effect Unit
setMarloweTokensProvider monaco languageId = runEffectFn3 setTokensProvider_ monaco languageId marloweTokensProvider
