{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Model.APPLICABILITY.Applicability where

import Language.Marlowe.ACTUS.Definitions.ContractTerms (ContractTerms(..), ContractType(..), ScheduleConfig(..), TermValidationError(..))
import Language.Marlowe.ACTUS.Model.APPLICABILITY.ApplicabilityModel
import Data.Maybe (isJust)
import Data.Validation

validateTerms :: ContractTerms -> Validation [TermValidationError] ContractTerms
validateTerms t =
    case contractType t of
        Just PAM -> 
            pure t <*
            _X (calendar . scfg) t "calendar" <*
            _X (eomc . scfg) t "end of month convention" <*
            _NN_I_1 [isJust $ ct_PRD t, isJust $ ct_PPRD t] t ["purchase date", "price at purchase date"] <*
            _NN_I_1 [isJust $ ct_TD t, isJust $ ct_PTD t] t ["termination date", "price at termination"]
        Just LAM ->
            pure t
        Nothing ->
            Failure [Required $ "Contract term 'contract type' is required."]