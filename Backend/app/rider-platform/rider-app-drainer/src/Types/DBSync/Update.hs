module Types.DBSync.Update where

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

-- Each update option contains a list of (key, value) pairs to set during
-- the update and a list of (key, value) pairs to use as a where clause.

data UpdateModel
  = AppInstallsUpdate
  | BlackListOrgUpdate
  | BookingUpdate
  | BookingLocationUpdate
  | BookingCancellationReasonUpdate
  | CallbackRequestUpdate
  | CallStatusUpdate
  | CancellationReasonUpdate
  | DriverOfferUpdate
  | EstimateUpdate
  | EstimateBreakupUpdate
  | ExophoneUpdate
  | FareBreakupUpdate
  | GeometryUpdate
  | IssueUpdate
  | DirectionsCacheUpdate
  | PlaceNameCacheUpdate
  | MerchantUpdate
  | MerchantMessageUpdate
  | MerchantPaymentMethodUpdate
  | MerchantServiceConfigUpdate
  | MerchantServiceUsageConfigUpdate
  | MerchantConfigUpdate
  | OnSearchEventUpdate
  | PaymentOrderUpdate
  | PaymentTransactionUpdate
  | PersonUpdate
  | PersonDefaultEmergencyNumberUpdate
  | PersonFlowStatusUpdate
  | QuoteUpdate
  | RegistrationTokenUpdate
  | RentalSlabUpdate
  | RideUpdate
  | SavedReqLocationUpdate
  | SearchRequestUpdate
  | SearchReqLocationUpdate
  | SosUpdate
  | SpecialZoneQuoteUpdate
  | TripTermsUpdate
  | WebengageUpdate
  | FeedbackFormUpdate
  | HotSpotConfigUpdate
  | BecknRequestUpdate
  deriving (Generic, Show)

getTagUpdate :: UpdateModel -> Text
getTagUpdate AppInstallsUpdate = "AppInstallsOptions"
getTagUpdate BlackListOrgUpdate = "BlackListOrgOptions"
getTagUpdate BookingUpdate = "BookingOptions"
getTagUpdate BookingLocationUpdate = "BookingLocationOptions"
getTagUpdate BookingCancellationReasonUpdate = "BookingCancellationReasonOptions"
getTagUpdate CallbackRequestUpdate = "CallbackRequestOptions"
getTagUpdate CallStatusUpdate = "CallStatusOptions"
getTagUpdate CancellationReasonUpdate = "CancellationReasonOptions"
getTagUpdate DriverOfferUpdate = "DriverOfferOptions"
getTagUpdate EstimateUpdate = "EstimateOptions"
getTagUpdate EstimateBreakupUpdate = "EstimateBreakupOptions"
getTagUpdate ExophoneUpdate = "ExophoneOptions"
getTagUpdate FareBreakupUpdate = "FareBreakupOptions"
getTagUpdate GeometryUpdate = "GeometryOptions"
getTagUpdate IssueUpdate = "IssueOptions"
getTagUpdate DirectionsCacheUpdate = "DirectionsCacheOptions"
getTagUpdate PlaceNameCacheUpdate = "PlaceNameCacheOptions"
getTagUpdate MerchantUpdate = "MerchantOptions"
getTagUpdate MerchantMessageUpdate = "MerchantMessageOptions"
getTagUpdate MerchantPaymentMethodUpdate = "MerchantPaymentMethodOptions"
getTagUpdate MerchantServiceConfigUpdate = "MerchantServiceConfigOptions"
getTagUpdate MerchantServiceUsageConfigUpdate = "MerchantServiceUsageConfigOptions"
getTagUpdate MerchantConfigUpdate = "MerchantConfigOptions"
getTagUpdate OnSearchEventUpdate = "OnSearchEventOptions"
getTagUpdate PaymentOrderUpdate = "PaymentOrderOptions"
getTagUpdate PaymentTransactionUpdate = "PaymentTransactionOptions"
getTagUpdate PersonUpdate = "PersonOptions"
getTagUpdate PersonDefaultEmergencyNumberUpdate = "PersonDefaultEmergencyNumberOptions"
getTagUpdate PersonFlowStatusUpdate = "PersonFlowStatusOptions"
getTagUpdate QuoteUpdate = "QuoteOptions"
getTagUpdate RegistrationTokenUpdate = "RegistrationTokenOptions"
getTagUpdate RentalSlabUpdate = "RentalSlabOptions"
getTagUpdate RideUpdate = "RideOptions"
getTagUpdate SavedReqLocationUpdate = "SavedReqLocationOptions"
getTagUpdate SearchRequestUpdate = "SearchRequestOptions"
getTagUpdate SearchReqLocationUpdate = "SearchReqLocationOptions"
getTagUpdate SosUpdate = "SosOptions"
getTagUpdate SpecialZoneQuoteUpdate = "SpecialZoneQuoteOptions"
getTagUpdate TripTermsUpdate = "TripTermsOptions"
getTagUpdate WebengageUpdate = "WebengageOptions"
getTagUpdate FeedbackFormUpdate = "FeedbackFormOptions"
getTagUpdate HotSpotConfigUpdate = "HotSpotConfigOptions"
getTagUpdate BecknRequestUpdate = "BecknRequestOptions"

parseTagUpdate :: Text -> Parser UpdateModel
parseTagUpdate "AppInstallsOptions" = return AppInstallsUpdate
parseTagUpdate "BlackListOrgOptions" = return BlackListOrgUpdate
parseTagUpdate "BookingOptions" = return BookingUpdate
parseTagUpdate "BookingLocationOptions" = return BookingLocationUpdate
parseTagUpdate "BookingCancellationReasonOptions" = return BookingCancellationReasonUpdate
parseTagUpdate "CallbackRequestOptions" = return CallbackRequestUpdate
parseTagUpdate "CallStatusOptions" = return CallStatusUpdate
parseTagUpdate "CancellationReasonOptions" = return CancellationReasonUpdate
parseTagUpdate "DriverOfferOptions" = return DriverOfferUpdate
parseTagUpdate "EstimateOptions" = return EstimateUpdate
parseTagUpdate "EstimateBreakupOptions" = return EstimateBreakupUpdate
parseTagUpdate "ExophoneOptions" = return ExophoneUpdate
parseTagUpdate "FareBreakupOptions" = return FareBreakupUpdate
parseTagUpdate "GeometryOptions" = return GeometryUpdate
parseTagUpdate "IssueOptions" = return IssueUpdate
parseTagUpdate "DirectionsCacheOptions" = return DirectionsCacheUpdate
parseTagUpdate "PlaceNameCacheOptions" = return PlaceNameCacheUpdate
parseTagUpdate "MerchantOptions" = return MerchantUpdate
parseTagUpdate "MerchantMessageOptions" = return MerchantMessageUpdate
parseTagUpdate "MerchantPaymentMethodOptions" = return MerchantPaymentMethodUpdate
parseTagUpdate "MerchantServiceConfigOptions" = return MerchantServiceConfigUpdate
parseTagUpdate "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigUpdate
parseTagUpdate "MerchantConfigOptions" = return MerchantConfigUpdate
parseTagUpdate "OnSearchEventOptions" = return OnSearchEventUpdate
parseTagUpdate "PaymentOrderOptions" = return PaymentOrderUpdate
parseTagUpdate "PaymentTransactionOptions" = return PaymentTransactionUpdate
parseTagUpdate "PersonOptions" = return PersonUpdate
parseTagUpdate "PersonDefaultEmergencyNumberOptions" = return PersonDefaultEmergencyNumberUpdate
parseTagUpdate "PersonFlowStatusOptions" = return PersonFlowStatusUpdate
parseTagUpdate "QuoteOptions" = return QuoteUpdate
parseTagUpdate "RegistrationTokenOptions" = return RegistrationTokenUpdate
parseTagUpdate "RentalSlabOptions" = return RentalSlabUpdate
parseTagUpdate "RideOptions" = return RideUpdate
parseTagUpdate "SavedReqLocationOptions" = return SavedReqLocationUpdate
parseTagUpdate "SearchRequestOptions" = return SearchRequestUpdate
parseTagUpdate "SearchReqLocationOptions" = return SearchReqLocationUpdate
parseTagUpdate "SosOptions" = return SosUpdate
parseTagUpdate "SpecialZoneQuoteOptions" = return SpecialZoneQuoteUpdate
parseTagUpdate "TripTermsOptions" = return TripTermsUpdate
parseTagUpdate "WebengageOptions" = return WebengageUpdate
parseTagUpdate "FeedbackFormOptions" = return FeedbackFormUpdate
parseTagUpdate "HotSpotConfigOptions" = return HotSpotConfigUpdate
parseTagUpdate "BecknRequestOptions" = return BecknRequestUpdate
parseTagUpdate t = fail $ T.unpack ("Expected a UpdateModel but got '" <> t <> "'")

data DBUpdateObject
  = AppInstallsOptions UpdateModel [Set Postgres AppInstalls.AppInstallsT] (Where Postgres AppInstalls.AppInstallsT)
  | BlackListOrgOptions UpdateModel [Set Postgres BlackListOrg.BlackListOrgT] (Where Postgres BlackListOrg.BlackListOrgT)
  | BookingOptions UpdateModel [Set Postgres Booking.BookingT] (Where Postgres Booking.BookingT)
  | BookingLocationOptions UpdateModel [Set Postgres BookingLocation.BookingLocationT] (Where Postgres BookingLocation.BookingLocationT)
  | BookingCancellationReasonOptions UpdateModel [Set Postgres BookingCancellationReason.BookingCancellationReasonT] (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | CallbackRequestOptions UpdateModel [Set Postgres CallbackRequest.CallbackRequestT] (Where Postgres CallbackRequest.CallbackRequestT)
  | CallStatusOptions UpdateModel [Set Postgres CallStatus.CallStatusT] (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonOptions UpdateModel [Set Postgres CancellationReason.CancellationReasonT] (Where Postgres CancellationReason.CancellationReasonT)
  | DriverOfferOptions UpdateModel [Set Postgres DriverOffer.DriverOfferT] (Where Postgres DriverOffer.DriverOfferT)
  | EstimateOptions UpdateModel [Set Postgres Estimate.EstimateT] (Where Postgres Estimate.EstimateT)
  | EstimateBreakupOptions UpdateModel [Set Postgres EstimateBreakup.EstimateBreakupT] (Where Postgres EstimateBreakup.EstimateBreakupT)
  | ExophoneOptions UpdateModel [Set Postgres Exophone.ExophoneT] (Where Postgres Exophone.ExophoneT)
  | FareBreakupOptions UpdateModel [Set Postgres FareBreakup.FareBreakupT] (Where Postgres FareBreakup.FareBreakupT)
  | GeometryOptions UpdateModel [Set Postgres Geometry.GeometryT] (Where Postgres Geometry.GeometryT)
  | IssueOptions UpdateModel [Set Postgres Issue.IssueT] (Where Postgres Issue.IssueT)
  | DirectionsCacheOptions UpdateModel [Set Postgres DirectionsCache.DirectionsCacheT] (Where Postgres DirectionsCache.DirectionsCacheT)
  | PlaceNameCacheOptions UpdateModel [Set Postgres PlaceNameCache.PlaceNameCacheT] (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MerchantOptions UpdateModel [Set Postgres Merchant.MerchantT] (Where Postgres Merchant.MerchantT)
  | MerchantMessageOptions UpdateModel [Set Postgres MerchantMessage.MerchantMessageT] (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodOptions UpdateModel [Set Postgres MerchantPaymentMethod.MerchantPaymentMethodT] (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigOptions UpdateModel [Set Postgres MerchantServiceConfig.MerchantServiceConfigT] (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigOptions UpdateModel [Set Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT] (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantConfigOptions UpdateModel [Set Postgres FraudConfig.FraudConfigT] (Where Postgres FraudConfig.FraudConfigT)
  | OnSearchEventOptions UpdateModel [Set Postgres OnSearchEvent.OnSearchEventT] (Where Postgres OnSearchEvent.OnSearchEventT)
  | PaymentOrderOptions UpdateModel [Set Postgres PaymentOrder.PaymentOrderT] (Where Postgres PaymentOrder.PaymentOrderT)
  | PaymentTransactionOptions UpdateModel [Set Postgres PaymentTransaction.PaymentTransactionT] (Where Postgres PaymentTransaction.PaymentTransactionT)
  | PersonOptions UpdateModel [Set Postgres Person.PersonT] (Where Postgres Person.PersonT)
  | PersonDefaultEmergencyNumberOptions UpdateModel [Set Postgres PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumberT] (Where Postgres PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumberT)
  | PersonFlowStatusOptions UpdateModel [Set Postgres PersonFlowStatus.PersonFlowStatusT] (Where Postgres PersonFlowStatus.PersonFlowStatusT)
  | QuoteOptions UpdateModel [Set Postgres Quote.QuoteT] (Where Postgres Quote.QuoteT)
  | RegistrationTokenOptions UpdateModel [Set Postgres RegistrationToken.RegistrationTokenT] (Where Postgres RegistrationToken.RegistrationTokenT)
  | RentalSlabOptions UpdateModel [Set Postgres RentalSlab.RentalSlabT] (Where Postgres RentalSlab.RentalSlabT)
  | RideOptions UpdateModel [Set Postgres Ride.RideT] (Where Postgres Ride.RideT)
  | SavedReqLocationOptions UpdateModel [Set Postgres SavedReqLocation.SavedReqLocationT] (Where Postgres SavedReqLocation.SavedReqLocationT)
  | SearchRequestOptions UpdateModel [Set Postgres SearchRequest.SearchRequestT] (Where Postgres SearchRequest.SearchRequestT)
  | SearchReqLocationOptions UpdateModel [Set Postgres SearchReqLocation.SearchReqLocationT] (Where Postgres SearchReqLocation.SearchReqLocationT)
  | SosOptions UpdateModel [Set Postgres Sos.SosT] (Where Postgres Sos.SosT)
  | SpecialZoneQuoteOptions UpdateModel [Set Postgres SpecialZoneQuote.SpecialZoneQuoteT] (Where Postgres SpecialZoneQuote.SpecialZoneQuoteT)
  | TripTermsOptions UpdateModel [Set Postgres TripTerms.TripTermsT] (Where Postgres TripTerms.TripTermsT)
  | WebengageOptions UpdateModel [Set Postgres Webengage.WebengageT] (Where Postgres Webengage.WebengageT)
  | FeedbackFormOptions UpdateModel [Set Postgres FeedbackForm.FeedbackFormT] (Where Postgres FeedbackForm.FeedbackFormT)
  | HotSpotConfigOptions UpdateModel [Set Postgres HotSpotConfig.HotSpotConfigT] (Where Postgres HotSpotConfig.HotSpotConfigT)
  | BecknRequestOptions UpdateModel [Set Postgres BecknRequest.BecknRequestT] (Where Postgres BecknRequest.BecknRequestT)

-------------------------------- ToJSON DBUpdateObject -------------------------------------
instance ToJSON DBUpdateObject where
  toJSON = error "ToJSON not implemented for DBUpdateObject - Use getDbUpdateCommandJson instead" -- Using getDbUpdateCommandJson instead of toJSON

-- -------------------------------- FromJSON DBUpdateObject -----------------------------------
instance FromJSON DBUpdateObject where
  parseJSON = A.withObject "DBUpdateObject" $ \o -> do
    contents <- o .: "contents"
    updateModel <- parseTagUpdate =<< (o .: "tag")
    case updateModel of
      AppInstallsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AppInstallsOptions updateModel updVals whereClause
      BlackListOrgUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BlackListOrgOptions updateModel updVals whereClause
      BookingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingOptions updateModel updVals whereClause
      BookingLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingLocationOptions updateModel updVals whereClause
      BookingCancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingCancellationReasonOptions updateModel updVals whereClause
      CallbackRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CallbackRequestOptions updateModel updVals whereClause
      CallStatusUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CallStatusOptions updateModel updVals whereClause
      CancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CancellationReasonOptions updateModel updVals whereClause
      DriverOfferUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverOfferOptions updateModel updVals whereClause
      EstimateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EstimateOptions updateModel updVals whereClause
      EstimateBreakupUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EstimateBreakupOptions updateModel updVals whereClause
      ExophoneUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ExophoneOptions updateModel updVals whereClause
      FareBreakupUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareBreakupOptions updateModel updVals whereClause
      GeometryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GeometryOptions updateModel updVals whereClause
      IssueUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueOptions updateModel updVals whereClause
      PlaceNameCacheUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PlaceNameCacheOptions updateModel updVals whereClause
      DirectionsCacheUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DirectionsCacheOptions updateModel updVals whereClause
      MerchantUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantOptions updateModel updVals whereClause
      MerchantMessageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantMessageOptions updateModel updVals whereClause
      MerchantPaymentMethodUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantPaymentMethodOptions updateModel updVals whereClause
      MerchantServiceConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantServiceConfigOptions updateModel updVals whereClause
      MerchantServiceUsageConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantServiceUsageConfigOptions updateModel updVals whereClause
      MerchantConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantConfigOptions updateModel updVals whereClause
      OnSearchEventUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OnSearchEventOptions updateModel updVals whereClause
      PaymentOrderUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentOrderOptions updateModel updVals whereClause
      PaymentTransactionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentTransactionOptions updateModel updVals whereClause
      PersonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonOptions updateModel updVals whereClause
      PersonDefaultEmergencyNumberUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonDefaultEmergencyNumberOptions updateModel updVals whereClause
      PersonFlowStatusUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonFlowStatusOptions updateModel updVals whereClause
      QuoteUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ QuoteOptions updateModel updVals whereClause
      RegistrationTokenUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RegistrationTokenOptions updateModel updVals whereClause
      RentalSlabUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RentalSlabOptions updateModel updVals whereClause
      RideUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RideOptions updateModel updVals whereClause
      SavedReqLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SavedReqLocationOptions updateModel updVals whereClause
      SearchRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchRequestOptions updateModel updVals whereClause
      SearchReqLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchReqLocationOptions updateModel updVals whereClause
      SosUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SosOptions updateModel updVals whereClause
      SpecialZoneQuoteUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SpecialZoneQuoteOptions updateModel updVals whereClause
      TripTermsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TripTermsOptions updateModel updVals whereClause
      WebengageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ WebengageOptions updateModel updVals whereClause
      FeedbackFormUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FeedbackFormOptions updateModel updVals whereClause
      HotSpotConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ HotSpotConfigOptions updateModel updVals whereClause
      BecknRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BecknRequestOptions updateModel updVals whereClause
