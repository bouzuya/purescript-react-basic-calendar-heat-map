module Component.App
  ( app
  ) where

import Prelude

import Bouzuya.DateTime (Date, DateTime(..), exactDateFromWeekOfYear)
import Component.AppStyle as Style
import Data.Array as Array
import Data.Enum (toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, createComponent, make)
import React.Basic.DOM (css)
import React.Basic.DOM as H
import React.Basic.DOM.Events (targetValue)
import Simple.JSON as SimpleJSON

type Props =
  {}

type State =
  { jsonObject :: Object Int
  , jsonText :: String
  }

data Action
  = UpdateJson String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { jsonObject: Object.empty
  , jsonText: "{}"
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
          [ H.text "App" ]
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
              ( flip Array.mapWithIndex
                ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
                (\weekdayNumber weekdayString ->
                  H.tr_
                  ( [ H.th
                      { className: Style.th
                      , children: [ H.text weekdayString ]
                      }
                    ] <>
                    ( (Array.range 1 53) <#>
                      (\weekNumber -> -- weekNumber
                        let
                          date :: Maybe Date
                          date = do
                            y <- toEnum 2019 -- TODO
                            woy <- toEnum weekNumber
                            dow <- toEnum weekdayNumber
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
