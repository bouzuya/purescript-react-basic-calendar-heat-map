module Component.App
  ( app
  ) where

import Prelude

import Bouzuya.DateTime (Date, WeekOfYear, Weekday, exactDateFromWeekOfYear)
import Component.AppStyle as Style
import Data.Array as Array
import Data.Enum (enumFromTo, toEnum)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Format as Format
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, createComponent, make)
import React.Basic.DOM (css)
import React.Basic.DOM as H
import React.Basic.DOM.Events (targetValue)
import Simple.JSON as SimpleJSON

type Props =
  {}

type State =
  { colors :: Array String
  , jsonObject :: Object Int
  , jsonText :: String
  }

data Action
  = UpdateJson String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialJSON :: Object Int
initialJSON =
  Object.fromFoldable
    [ Tuple "2019-01-01" 0
    , Tuple "2019-01-02" 1
    , Tuple "2019-01-03" 2
    ]

initialState :: State
initialState =
  { colors: ["#eee", "#999", "#333"]
  , jsonObject: initialJSON
  , jsonText: SimpleJSON.writeJSON initialJSON
  }

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "Calendar Heat Map" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ H.label_
          [ H.span_ [ H.text "data" ]
          , H.textarea
            { onChange:
                capture
                  self
                  targetValue
                  (\v -> UpdateJson (fromMaybe "" v))
            , value: self.state.jsonText
            }
          ]
        , H.table
          { className: Style.table
          , children:
            [ H.thead_
              []
            , H.tbody_
              (
                (enumFromTo bottom top :: Array Weekday) <#>
                (\dow ->
                  H.tr_
                  ( [ H.th
                      { className: Style.th
                      , children: [ H.text (Format.dayOfWeekShortName dow) ]
                      }
                    ] <>
                    ( (enumFromTo bottom top :: Array WeekOfYear) <#>
                      (\woy ->
                        let
                          colorIndex = fromMaybe 0 do
                            y <- toEnum 2019 -- TODO
                            d <- exactDateFromWeekOfYear y woy dow
                            k <- pure (Format.iso8601Date d)
                            Object.lookup k self.state.jsonObject
                          color v =
                            fromMaybe
                              "transparent"
                              (Array.index self.state.colors v)
                        in
                          H.td
                          { className: Style.td
                          , children:
                            [ H.span
                              { className: Style.value
                              , children: [ H.text (show colorIndex) ]
                              , style: css { backgroundColor: color colorIndex }
                              }
                            ]
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
update self (UpdateJson s) =
  Update
    (self.state
      { jsonObject = fromMaybe Object.empty (SimpleJSON.readJSON_ s)
      , jsonText = s
      })
