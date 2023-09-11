{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Types.DBSync.Delete where

import Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import qualified Kernel.Storage.Beam.BecknRequestRider as BecknRequest
import Sequelize
import qualified "rider-app" Storage.Beam.AppInstalls as AppInstalls
import qualified "rider-app" Storage.Beam.BlackListOrg as BlackListOrg
import qualified "rider-app" Storage.Beam.Booking as Booking
import qualified "rider-app" Storage.Beam.Booking.BookingLocation as BookingLocation
import qualified "rider-app" Storage.Beam.BookingCancellationReason as BookingCancellationReason
import qualified "rider-app" Storage.Beam.CallStatus as CallStatus
import qualified "rider-app" Storage.Beam.CallbackRequest as CallbackRequest
import qualified "rider-app" Storage.Beam.CancellationReason as CancellationReason
import qualified "rider-app" Storage.Beam.DriverOffer as DriverOffer
import qualified "rider-app" Storage.Beam.Estimate as Estimate
import qualified "rider-app" Storage.Beam.EstimateBreakup as EstimateBreakup
import qualified "rider-app" Storage.Beam.Exophone as Exophone
import qualified "rider-app" Storage.Beam.FarePolicy.FareBreakup as FareBreakup
import qualified "rider-app" Storage.Beam.FeedbackForm as FeedbackForm
import qualified "rider-app" Storage.Beam.FraudConfig as FraudConfig
import qualified "rider-app" Storage.Beam.Geometry as Geometry
import qualified "rider-app" Storage.Beam.HotSpotConfig as HotSpotConfig
import qualified "rider-app" Storage.Beam.Issue as Issue
import qualified "rider-app" Storage.Beam.Maps.DirectionsCache as DirectionsCache
import qualified "rider-app" Storage.Beam.Maps.PlaceNameCache as PlaceNameCache
import qualified "rider-app" Storage.Beam.Merchant as Merchant
import qualified "rider-app" Storage.Beam.Merchant.MerchantMessage as MerchantMessage
import qualified "rider-app" Storage.Beam.Merchant.MerchantPaymentMethod as MerchantPaymentMethod
import qualified "rider-app" Storage.Beam.Merchant.MerchantServiceConfig as MerchantServiceConfig
import qualified "rider-app" Storage.Beam.Merchant.MerchantServiceUsageConfig as MerchantServiceUsageConfig
import qualified "rider-app" Storage.Beam.OnSearchEvent as OnSearchEvent
import qualified "rider-app" Storage.Beam.Payment.PaymentOrder as PaymentOrder
import qualified "rider-app" Storage.Beam.Payment.PaymentTransaction as PaymentTransaction
import qualified "rider-app" Storage.Beam.Person as Person
import qualified "rider-app" Storage.Beam.Person.PersonDefaultEmergencyNumber as PersonDefaultEmergencyNumber
import qualified "rider-app" Storage.Beam.Person.PersonFlowStatus as PersonFlowStatus
import qualified "rider-app" Storage.Beam.Quote as Quote
import qualified "rider-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "rider-app" Storage.Beam.RentalSlab as RentalSlab
import qualified "rider-app" Storage.Beam.Ride as Ride
import qualified "rider-app" Storage.Beam.SavedReqLocation as SavedReqLocation
import qualified "rider-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "rider-app" Storage.Beam.SearchRequest.SearchReqLocation as SearchReqLocation
import qualified "rider-app" Storage.Beam.Sos as Sos
import qualified "rider-app" Storage.Beam.SpecialZoneQuote as SpecialZoneQuote
import qualified "rider-app" Storage.Beam.TripTerms as TripTerms
import qualified "rider-app" Storage.Beam.Webengage as Webengage
import Utils.Parse

data DeleteModel
  = AppInstallsDelete
  | BlackListOrgDelete
  | BookingDelete
  | BookingLocationDelete
  | BookingCancellationReasonDelete
  | CallbackRequestDelete
  | CallStatusDelete
  | CancellationReasonDelete
  | DriverOfferDelete
  | EstimateDelete
  | EstimateBreakupDelete
  | ExophoneDelete
  | FareBreakupDelete
  | GeometryDelete
  | IssueDelete
  | DirectionsCacheDelete
  | PlaceNameCacheDelete
  | MerchantDelete
  | MerchantMessageDelete
  | MerchantPaymentMethodDelete
  | MerchantServiceConfigDelete
  | MerchantServiceUsageConfigDelete
  | MerchantConfigDelete
  | OnSearchEventDelete
  | PaymentOrderDelete
  | PaymentTransactionDelete
  | PersonDelete
  | PersonDefaultEmergencyNumberDelete
  | PersonFlowStatusDelete
  | QuoteDelete
  | RegistrationTokenDelete
  | RentalSlabDelete
  | RideDelete
  | SavedReqLocationDelete
  | SearchRequestDelete
  | SearchReqLocationDelete
  | SosDelete
  | SpecialZoneQuoteDelete
  | TripTermsDelete
  | WebengageDelete
  | FeedbackFormDelete
  | HotSpotConfigDelete
  | BecknRequestDelete
  deriving (Generic, Show)

getTagDelete :: DeleteModel -> Text
getTagDelete AppInstallsDelete = "AppInstallsOptions"
getTagDelete BlackListOrgDelete = "BlackListOrgOptions"
getTagDelete BookingDelete = "BookingOptions"
getTagDelete BookingLocationDelete = "BookingLocationOptions"
getTagDelete BookingCancellationReasonDelete = "BookingCancellationReasonOptions"
getTagDelete CallbackRequestDelete = "CallbackRequestOptions"
getTagDelete CallStatusDelete = "CallStatusOptions"
getTagDelete CancellationReasonDelete = "CancellationReasonOptions"
getTagDelete DriverOfferDelete = "DriverOfferOptions"
getTagDelete EstimateDelete = "EstimateOptions"
getTagDelete EstimateBreakupDelete = "EstimateBreakupOptions"
getTagDelete ExophoneDelete = "ExophoneOptions"
getTagDelete FareBreakupDelete = "FareBreakupOptions"
getTagDelete GeometryDelete = "GeometryOptions"
getTagDelete IssueDelete = "IssueOptions"
getTagDelete DirectionsCacheDelete = "DirectionsCacheOptions"
getTagDelete PlaceNameCacheDelete = "PlaceNameCacheOptions"
getTagDelete MerchantDelete = "MerchantOptions"
getTagDelete MerchantMessageDelete = "MerchantMessageOptions"
getTagDelete MerchantPaymentMethodDelete = "MerchantPaymentMethodOptions"
getTagDelete MerchantServiceConfigDelete = "MerchantServiceConfigOptions"
getTagDelete MerchantServiceUsageConfigDelete = "MerchantServiceUsageConfigOptions"
getTagDelete MerchantConfigDelete = "MerchantConfigOptions"
getTagDelete OnSearchEventDelete = "OnSearchEventOptions"
getTagDelete PaymentOrderDelete = "PaymentOrderOptions"
getTagDelete PaymentTransactionDelete = "PaymentTransactionOptions"
getTagDelete PersonDelete = "PersonOptions"
getTagDelete PersonDefaultEmergencyNumberDelete = "PersonDefaultEmergencyNumberOptions"
getTagDelete PersonFlowStatusDelete = "PersonFlowStatusOptions"
getTagDelete QuoteDelete = "QuoteOptions"
getTagDelete RegistrationTokenDelete = "RegistrationTokenOptions"
getTagDelete RentalSlabDelete = "RentalSlabOptions"
getTagDelete RideDelete = "RideOptions"
getTagDelete SavedReqLocationDelete = "SavedReqLocationOptions"
getTagDelete SearchRequestDelete = "SearchRequestOptions"
getTagDelete SearchReqLocationDelete = "SearchReqLocationOptions"
getTagDelete SosDelete = "SosOptions"
getTagDelete SpecialZoneQuoteDelete = "SpecialZoneQuoteOptions"
getTagDelete TripTermsDelete = "TripTermsOptions"
getTagDelete WebengageDelete = "WebengageOptions"
getTagDelete FeedbackFormDelete = "FeedbackFormOptions"
getTagDelete HotSpotConfigDelete = "HotSpotConfigOptions"
getTagDelete BecknRequestDelete = "BecknRequestOptions"

parseTagDelete :: Text -> Parser DeleteModel
parseTagDelete "AppInstallsOptions" = return AppInstallsDelete
parseTagDelete "BlackListOrgOptions" = return BlackListOrgDelete
parseTagDelete "BookingOptions" = return BookingDelete
parseTagDelete "BookingLocationOptions" = return BookingLocationDelete
parseTagDelete "BookingCancellationReasonOptions" = return BookingCancellationReasonDelete
parseTagDelete "CallbackRequestOptions" = return CallbackRequestDelete
parseTagDelete "CallStatusOptions" = return CallStatusDelete
parseTagDelete "CancellationReasonOptions" = return CancellationReasonDelete
parseTagDelete "DriverOfferOptions" = return DriverOfferDelete
parseTagDelete "EstimateOptions" = return EstimateDelete
parseTagDelete "EstimateBreakupOptions" = return EstimateBreakupDelete
parseTagDelete "ExophoneOptions" = return ExophoneDelete
parseTagDelete "FareBreakupOptions" = return FareBreakupDelete
parseTagDelete "GeometryOptions" = return GeometryDelete
parseTagDelete "IssueOptions" = return IssueDelete
parseTagDelete "DirectionsCacheOptions" = return DirectionsCacheDelete
parseTagDelete "PlaceNameCacheOptions" = return PlaceNameCacheDelete
parseTagDelete "MerchantOptions" = return MerchantDelete
parseTagDelete "MerchantMessageOptions" = return MerchantMessageDelete
parseTagDelete "MerchantPaymentMethodOptions" = return MerchantPaymentMethodDelete
parseTagDelete "MerchantServiceConfigOptions" = return MerchantServiceConfigDelete
parseTagDelete "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigDelete
parseTagDelete "MerchantConfigOptions" = return MerchantConfigDelete
parseTagDelete "OnSearchEventOptions" = return OnSearchEventDelete
parseTagDelete "PaymentOrderOptions" = return PaymentOrderDelete
parseTagDelete "PaymentTransactionOptions" = return PaymentTransactionDelete
parseTagDelete "PersonOptions" = return PersonDelete
parseTagDelete "PersonDefaultEmergencyNumberOptions" = return PersonDefaultEmergencyNumberDelete
parseTagDelete "PersonFlowStatusOptions" = return PersonFlowStatusDelete
parseTagDelete "QuoteOptions" = return QuoteDelete
parseTagDelete "RegistrationTokenOptions" = return RegistrationTokenDelete
parseTagDelete "RentalSlabOptions" = return RentalSlabDelete
parseTagDelete "RideOptions" = return RideDelete
parseTagDelete "SavedReqLocationOptions" = return SavedReqLocationDelete
parseTagDelete "SearchRequestOptions" = return SearchRequestDelete
parseTagDelete "SearchReqLocationOptions" = return SearchReqLocationDelete
parseTagDelete "SosOptions" = return SosDelete
parseTagDelete "SpecialZoneQuoteOptions" = return SpecialZoneQuoteDelete
parseTagDelete "TripTermsOptions" = return TripTermsDelete
parseTagDelete "WebengageOptions" = return WebengageDelete
parseTagDelete "FeedbackFormOptions" = return FeedbackFormDelete
parseTagDelete "HotSpotConfigOptions" = return HotSpotConfigDelete
parseTagDelete "BecknRequestOptions" = return BecknRequestDelete
parseTagDelete t = fail $ T.unpack ("Expected a DeleteModel but got '" <> t <> "'")

data DBDeleteObject
  = AppInstallsDeleteOptions DeleteModel (Where Postgres AppInstalls.AppInstallsT)
  | BlackListOrgDeleteOptions DeleteModel (Where Postgres BlackListOrg.BlackListOrgT)
  | BookingDeleteOptions DeleteModel (Where Postgres Booking.BookingT)
  | BookingLocationDeleteOptions DeleteModel (Where Postgres BookingLocation.BookingLocationT)
  | BookingCancellationReasonDeleteOptions DeleteModel (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | CallbackRequestDeleteOptions DeleteModel (Where Postgres CallbackRequest.CallbackRequestT)
  | CallStatusDeleteOptions DeleteModel (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonDeleteOptions DeleteModel (Where Postgres CancellationReason.CancellationReasonT)
  | DriverOfferDeleteOptions DeleteModel (Where Postgres DriverOffer.DriverOfferT)
  | EstimateDeleteOptions DeleteModel (Where Postgres Estimate.EstimateT)
  | EstimateBreakupDeleteOptions DeleteModel (Where Postgres EstimateBreakup.EstimateBreakupT)
  | ExophoneDeleteOptions DeleteModel (Where Postgres Exophone.ExophoneT)
  | FareBreakupDeleteOptions DeleteModel (Where Postgres FareBreakup.FareBreakupT)
  | GeometryDeleteOptions DeleteModel (Where Postgres Geometry.GeometryT)
  | IssueDeleteOptions DeleteModel (Where Postgres Issue.IssueT)
  | DirectionsCacheDeleteOptions DeleteModel (Where Postgres DirectionsCache.DirectionsCacheT)
  | PlaceNameCacheDeleteOptions DeleteModel (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MerchantDeleteOptions DeleteModel (Where Postgres Merchant.MerchantT)
  | MerchantMessageDeleteOptions DeleteModel (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodDeleteOptions DeleteModel (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantConfigDeleteOptions DeleteModel (Where Postgres FraudConfig.FraudConfigT)
  | OnSearchEventDeleteOptions DeleteModel (Where Postgres OnSearchEvent.OnSearchEventT)
  | PaymentOrderDeleteOptions DeleteModel (Where Postgres PaymentOrder.PaymentOrderT)
  | PaymentTransactionDeleteOptions DeleteModel (Where Postgres PaymentTransaction.PaymentTransactionT)
  | PersonDeleteOptions DeleteModel (Where Postgres Person.PersonT)
  | PersonDefaultEmergencyNumberDeleteOptions DeleteModel (Where Postgres PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumberT)
  | PersonFlowStatusDeleteOptions DeleteModel (Where Postgres PersonFlowStatus.PersonFlowStatusT)
  | QuoteDeleteOptions DeleteModel (Where Postgres Quote.QuoteT)
  | RegistrationTokenDeleteOptions DeleteModel (Where Postgres RegistrationToken.RegistrationTokenT)
  | RentalSlabDeleteOptions DeleteModel (Where Postgres RentalSlab.RentalSlabT)
  | RideDeleteOptions DeleteModel (Where Postgres Ride.RideT)
  | SavedReqLocationDeleteOptions DeleteModel (Where Postgres SavedReqLocation.SavedReqLocationT)
  | SearchRequestDeleteOptions DeleteModel (Where Postgres SearchRequest.SearchRequestT)
  | SearchReqLocationDeleteOptions DeleteModel (Where Postgres SearchReqLocation.SearchReqLocationT)
  | SosDeleteOptions DeleteModel (Where Postgres Sos.SosT)
  | SpecialZoneQuoteDeleteOptions DeleteModel (Where Postgres SpecialZoneQuote.SpecialZoneQuoteT)
  | TripTermsDeleteOptions DeleteModel (Where Postgres TripTerms.TripTermsT)
  | WebengageDeleteOptions DeleteModel (Where Postgres Webengage.WebengageT)
  | FeedbackFormDeleteOptions DeleteModel (Where Postgres FeedbackForm.FeedbackFormT)
  | HotSpotConfigDeleteOptions DeleteModel (Where Postgres HotSpotConfig.HotSpotConfigT)
  | BecknRequestDeleteOptions DeleteModel (Where Postgres BecknRequest.BecknRequestT)

instance ToJSON DBDeleteObject where
  toJSON = error "ToJSON not implemented for DBDeleteObject - Use getDbDeleteCommandJson instead" -- Using getDbDeleteCommandJson instead of toJSON

instance FromJSON DBDeleteObject where
  parseJSON = A.withObject "DBDeleteObject" $ \o -> do
    contents <- o .: "contents"
    deleteModel <- parseTagDelete =<< (o .: "tag")
    case deleteModel of
      AppInstallsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AppInstallsDeleteOptions deleteModel whereClause
      BlackListOrgDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BlackListOrgDeleteOptions deleteModel whereClause
      BookingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingDeleteOptions deleteModel whereClause
      BookingLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingLocationDeleteOptions deleteModel whereClause
      BookingCancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingCancellationReasonDeleteOptions deleteModel whereClause
      CallbackRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CallbackRequestDeleteOptions deleteModel whereClause
      CallStatusDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CallStatusDeleteOptions deleteModel whereClause
      CancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CancellationReasonDeleteOptions deleteModel whereClause
      DriverOfferDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverOfferDeleteOptions deleteModel whereClause
      EstimateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EstimateDeleteOptions deleteModel whereClause
      EstimateBreakupDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EstimateBreakupDeleteOptions deleteModel whereClause
      ExophoneDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ExophoneDeleteOptions deleteModel whereClause
      FareBreakupDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareBreakupDeleteOptions deleteModel whereClause
      GeometryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GeometryDeleteOptions deleteModel whereClause
      IssueDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueDeleteOptions deleteModel whereClause
      PlaceNameCacheDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PlaceNameCacheDeleteOptions deleteModel whereClause
      DirectionsCacheDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DirectionsCacheDeleteOptions deleteModel whereClause
      MerchantDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantDeleteOptions deleteModel whereClause
      MerchantMessageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantMessageDeleteOptions deleteModel whereClause
      MerchantPaymentMethodDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantPaymentMethodDeleteOptions deleteModel whereClause
      MerchantServiceConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantServiceConfigDeleteOptions deleteModel whereClause
      MerchantServiceUsageConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantServiceUsageConfigDeleteOptions deleteModel whereClause
      MerchantConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantConfigDeleteOptions deleteModel whereClause
      OnSearchEventDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OnSearchEventDeleteOptions deleteModel whereClause
      PaymentOrderDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentOrderDeleteOptions deleteModel whereClause
      PaymentTransactionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentTransactionDeleteOptions deleteModel whereClause
      PersonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonDeleteOptions deleteModel whereClause
      PersonDefaultEmergencyNumberDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonDefaultEmergencyNumberDeleteOptions deleteModel whereClause
      PersonFlowStatusDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonFlowStatusDeleteOptions deleteModel whereClause
      QuoteDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ QuoteDeleteOptions deleteModel whereClause
      RegistrationTokenDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RegistrationTokenDeleteOptions deleteModel whereClause
      RentalSlabDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RentalSlabDeleteOptions deleteModel whereClause
      RideDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RideDeleteOptions deleteModel whereClause
      SavedReqLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SavedReqLocationDeleteOptions deleteModel whereClause
      SearchRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestDeleteOptions deleteModel whereClause
      SearchReqLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchReqLocationDeleteOptions deleteModel whereClause
      SosDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SosDeleteOptions deleteModel whereClause
      SpecialZoneQuoteDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SpecialZoneQuoteDeleteOptions deleteModel whereClause
      TripTermsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TripTermsDeleteOptions deleteModel whereClause
      WebengageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ WebengageDeleteOptions deleteModel whereClause
      FeedbackFormDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackFormDeleteOptions deleteModel whereClause
      HotSpotConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ HotSpotConfigDeleteOptions deleteModel whereClause
      BecknRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BecknRequestDeleteOptions deleteModel whereClause
