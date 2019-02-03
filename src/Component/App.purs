module Component.App
  ( app
  ) where

import Prelude

import Component.AppStyle as Style
import Data.Array as Array
import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make)
import React.Basic.DOM (css)
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
        , H.table
          { className: Style.table
          , children:
            [ H.thead_
              []
            , H.tbody_
              ( flip Array.mapWithIndex
                ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
                (\weekdayNumber weekdayString ->
                  H.tr_
                  ( [ H.th
                      { className: Style.th
                      , children: [ H.text weekdayString ]
                      }
                    ] <>
                    ( (Array.range 0 53) <#>
                      (\_ -> -- weekNumber
                        let
                          value = 0
                          color v = case v of
                            0 -> "#f00"
                            _ -> "transparent"
                        in
                          H.td
                          { className: Style.td
                          , children:
                            [ H.span
                              { className: Style.value
                              , children: [ H.text (show value) ]
                              , style: css { backgroundColor: color value }
                              }
                            ] -- data[weekNumber][weekdayNumber]
                          }
                      )
                    )
                  )
                )
              )
            ]
          }
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self Noop = NoUpdate
