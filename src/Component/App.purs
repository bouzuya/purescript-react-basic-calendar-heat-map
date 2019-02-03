module Component.App
  ( app
  ) where

import Prelude

import Data.Array as Array
import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make)
import React.Basic.DOM as H

type Props =
  {}

type State =
  {}

data Action
  = Noop

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  {}

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "App" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ H.label_
          [ H.span_ [ H.text "data" ]
          , H.textarea_ []
          ]
        , H.table_
          [ H.thead_
            []
          , H.tbody_
            ( flip Array.mapWithIndex
              ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
              (\weekdayNumber weekdayString ->
                H.tr_
                ( [ H.th_ [ H.text weekdayString ] ] <>
                  ( (Array.range 0 53) <#>
                    (\_ -> -- weekNumber
                      H.td_
                      [ H.span_ [ H.text "0" ] ] -- data[weekNumber][weekdayNumber]
                    )
                  )
                )
              )
            )
          ]
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self Noop = NoUpdate
