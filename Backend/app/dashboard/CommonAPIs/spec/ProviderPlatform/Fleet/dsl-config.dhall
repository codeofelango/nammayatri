let common = ../provider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes = defaultOutput._apiRelatedTypes ++ "/Fleet"
          , _servantApi = defaultOutput._servantApi ++ "/Fleet"
          , _domainHandler = defaultOutput._domainHandler + "/Fleet"
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/Fleet"
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/Fleet"
          }

let clientFunction =
      Some
        "ProviderPlatformClient.DynamicOfferDriver.Fleet.callDynamicOfferDriverAppFleetApi"

in      common.defaultConfigs
    //  { _output = outputPath, _clientFunction = clientFunction }
