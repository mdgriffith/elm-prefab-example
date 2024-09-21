module App.Resources exposing (..)

import App.Page.Id
import App.View


type alias Resources =
    { viewing : App.View.Regions App.Page.Id.Id }