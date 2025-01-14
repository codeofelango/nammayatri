{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverProfileQuestions where

import qualified API.Types.UI.DriverProfileQuestions
import Data.OpenApi (ToSchema)
import qualified Domain.Types.DriverProfileQuestions as DTDPQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.Queries.DriverProfileQuestions as DPQ
import Tools.Auth

postDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq ->
    Flow APISuccess
  )
postDriverProfileQues (mbPersonId, merchantId, merchantOpCityId) API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq {..} = do
  driverId <- B.runInReplica $ mbPersonId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  DPQ.upsert $
    DTDPQ.DriverProfileQuestions
      { aspirations = fromMaybe [] aspirations,
        createdAt = now,
        updatedAt = now,
        driverId = driverId,
        expertAt = fromMaybe [] expertAt,
        hometown = hometown,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        pledges = fromMaybe [] pledges,
        whyNY = fromMaybe [] whyNY
      }
  pure Success

getDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
  )
getDriverProfileQues (mbPersonId, _merchantId, _merchantOpCityId) = do
  driverId <- B.runInReplica $ mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbRes <- DPQ.findByPersonId driverId
  let defaultProfile =
        API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
          { aspirations = [],
            expertAt = [],
            hometown = Nothing,
            pledges = [],
            whyNY = []
          }
  case mbRes of
    Just res ->
      pure $
        API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
          { aspirations = res.aspirations,
            expertAt = res.expertAt,
            hometown = res.hometown,
            pledges = res.pledges,
            whyNY = res.whyNY
          }
    Nothing ->
      pure defaultProfile
