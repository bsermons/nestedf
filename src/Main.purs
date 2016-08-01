module Main where

import Prelude
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Tickers as Tickers
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))
import Halogen.Util (runHalogenAff, awaitBody)
import Network.HTTP.Affjax (AJAX, get)

data Query a
  = Init a

type State = { error :: String }

type MainSlot = Unit

type AppEffects eff = H.HalogenEffects (ajax :: AJAX | eff)
type State' g = H.ParentState State (Tickers.State' g) Query Tickers.Query' g MainSlot
type Query' = Coproduct Query (H.ChildF MainSlot Tickers.Query')

ui :: forall g. H.Component (State' (Aff (AppEffects g))) Query' (Aff (AppEffects g))
ui = H.parentComponent { render, eval, peek: Nothing }
  where

  -- render :: State -> H.ParentHTML TickState Query TickQuery g TickSlot
  render :: State -> H.ParentHTML (Tickers.State' (Aff (AppEffects g))) Query Tickers.Query' (Aff (AppEffects g)) MainSlot
  render st =
    HH.div_
      [ HH.slot unit (\_ -> { component: Tickers.ui
                            , initialState: H.parentState {tickA: Nothing, tickB: Nothing }
                            })
      ]

  eval :: Query ~> H.ParentDSL State (Tickers.State' (Aff (AppEffects g))) Query Tickers.Query' (Aff (AppEffects g)) MainSlot
  eval (Init next) = do
    resp <- H.fromAff $ get "/"
    pure $ queryG unit $ H.action (Tickers.ProxyA resp.response)
    pure next

  queryG
    :: forall s s' f f' p i
    . (Ord p)
    => p
    -> f' i
    -> Free (H.HalogenFP H.ParentEventSource s f (H.QueryF s s' f f' (Aff (AppEffects g)) p)) (Maybe i)
  queryG = H.query

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (H.parentState { error: "" }) body
