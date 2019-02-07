module Component.App
  ( app
  ) where

import Prelude

import Bouzuya.DateTime (Date, WeekOfYear, Weekday, Year, exactDateFromWeekOfYear)
import Component.AppStyle as Style
import Data.Array as Array
import Data.Enum (Cardinality(..), cardinality, enumFromTo, fromEnum, toEnum)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Int as Int
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Newtype (un, unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Format as Format
import Partial.Unsafe (unsafePartial)
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
  , year :: Year
  }

data Action
  = UpdateJson String

lookupValue :: Year -> WeekOfYear -> Weekday -> Object Int -> Maybe Int
lookupValue y woy dow obj = do
  d <- exactDateFromWeekOfYear y woy dow
  Object.lookup (Format.iso8601Date d) obj

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
  , year: unsafePartial (fromJust (toEnum 2019))
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
              [ H.tr_
                [ H.td
                  { children: [ H.text (show (fromEnum self.state.year)) ]
                  , className: Style.tableTitle
                  , colSpan: Int.toNumber (1 + unwrap (cardinality :: Cardinality WeekOfYear))
                  }
                ]
              ]
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
                            lookupValue
                              self.state.year
                              woy
                              dow
                              self.state.jsonObject
                          color = fromMaybe "transparent"
                            (Array.index self.state.colors colorIndex)
                        in
                          H.td
                          { className: Style.td
                          , children:
                            [ H.span
                              { className: Style.value
                              , children: [ H.text (show colorIndex) ]
                              , style: css { backgroundColor: color }
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
