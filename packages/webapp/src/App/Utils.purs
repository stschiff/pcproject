module App.Utils where

data RemoteData e a = NotAsked | Loading | Failure e | Success a