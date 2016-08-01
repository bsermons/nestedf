module Ticker where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE

type TickState =
  { count :: Int
  , data :: String
  }

data TickQuery a
  = Tick a
  | GetTick (Int -> a)
  | Load String a

ticker :: forall g. H.Component TickState TickQuery g
ticker = H.component { render, eval }
  where

  render :: TickState -> H.ComponentHTML TickQuery
  render state =
    HH.div_
      [ HH.h1
          [ HP.id_ "header" ]
          [ HH.text "counter" ]
      , HH.p_
          [ HH.text (show state.count) ]
      , HH.button
          [ HE.onClick (HE.input_ Tick) ]
          [ HH.text "Tick" ]
      ]

  eval :: TickQuery ~> H.ComponentDSL TickState TickQuery g
  eval (Tick next) = do
    cnt <- H.gets _.count
    H.modify _{ count = cnt + 1 }
    pure next
  eval (GetTick continue) = do
    n <- H.gets _.count
    pure (continue n)
  eval (Load contents next) = do
    H.modify _{ data = contents }
    pure next
