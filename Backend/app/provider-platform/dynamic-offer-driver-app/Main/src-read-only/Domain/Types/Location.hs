{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Location (module Domain.Types.Location, module ReExport) where

import Data.Aeson
import qualified Data.OpenApi
import Domain.Types.Extra.Location as ReExport
import qualified Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Location = Location
  { address :: Domain.Types.Location.LocationAddress,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Location.Location,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, Kernel.External.Maps.HasCoordinates.HasCoordinates, ToJSON, FromJSON, ToSchema)

data LocationAPIEntity = LocationAPIEntity
  { area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fullAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, Data.OpenApi.ToSchema)

data LocationAddress = LocationAddress
  { area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fullAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, Data.OpenApi.ToSchema)
