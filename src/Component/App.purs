module Component.App
  ( app
  ) where

import Prelude

import Bouzuya.DateTime (Date, DateTime(..), Weekday, exactDateFromWeekOfYear)
import Component.AppStyle as Style
import Data.Array as Array
import Data.Enum (enumFromTo, toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Format (dayOfWeekShortName)
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
                      , children: [ H.text (dayOfWeekShortName dow) ]
                      }
                    ] <>
                    ( (Array.range 1 53) <#>
                      (\weekNumber -> -- weekNumber
                        let
                          date :: Maybe Date
                          date = do
                            y <- toEnum 2019 -- TODO
                            woy <- toEnum weekNumber
                            exactDateFromWeekOfYear y woy dow
                          dateString :: Date -> String
                          dateString d =
                            format
                              (List.fromFoldable
                                [ YearFull
                                , Placeholder "-"
                                , MonthTwoDigits
                                , Placeholder "-"
                                , DayOfMonthTwoDigits
                                ])
                            (DateTime d bottom)
                          value = fromMaybe 0 do
                            d <- date
                            Object.lookup (dateString d) self.state.jsonObject
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
                              , children: [ H.text (show value) ]
                              , style: css { backgroundColor: color value }
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
