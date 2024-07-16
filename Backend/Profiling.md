<!-- Author : Vijay Gupta -->

haskell-flake < 0.5 (I believe NY is still on this)
# In Backend/default.nix
# -- In haskellProjects.default
{
  defaults.settings.default = {
    libraryProfiling = true;
  };
}
haskell-flake >= 0.5
# In Backend/default.nix
# -- In haskellProjects.default
{
  defaults.settings.defined = {
    libraryProfiling = true;
  };
}

- re-enter the devShell and ```run cabal <package_name> run --enable-profiling -- +RTS -p -RTS``` to generate the .prof  file


# in case of forcing profiling use this

defaults.settings.defined = {
          libraryProfiling = lib.mkForce true;
        };


# current setup has some issues with running all services with profiling.
# To solve that i ran all the services and ran the service i wanted to profile with profiling enabled manually
# This is a temporary solution and will be fixed in the future
```
cabal run dynamic-offer-driver-app  --enable-profiling -- +RTS -p -RTS
cabal run rider-app  --enable-profiling -- +RTS -p -RTS
```

# or for more detailed profiling
```
cabal run dynamic-offer-driver-app  --enable-profiling -- +RTS -hc -Pa -RTS
cabal run rider-app  --enable-profiling -- +RTS -hc -Pa -RTS
```