{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- module Domain.Action.Internal.VehicleService where

-- import Domain.Types.Merchant (Merchant)
-- import Environment
-- import EulerHS.Prelude hiding (id)
-- import Kernel.Types.APISuccess
-- import Kernel.Types.Beckn.Context as Context
-- import Servant

-- newtype VehicleServiceResp = VehicleServiceResp
--   { allServices = [VehicleService]
--   }
--   deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- data VehicleService = VehicleService
--   { serviceTierName :: Text,
--     vehicleImageUrl :: Text,
--     selectByDefault :: Bool
--   } deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- getVehicleService :: Id Merchant -> Context.City -> FlowHandler Domain.VehicleServiceResp
-- getVehicleService _ _ = dummyVehicleServiceResp

-- dummyVehicleServiceResp :: VehicleServiceResp
-- dummyVehicleServiceResp = VehicleServiceResp{allServices = []}
