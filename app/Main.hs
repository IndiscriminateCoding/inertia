module Main( main ) where

import Data.Version( showVersion )
import Paths_inertia( version )
import System.IO( stderr )

import qualified Options.Applicative as O
import qualified System.Log.Formatter as LF
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger as L

import Ast( listenerRules )
import Config( AdsConfig(..), Config(..), fileConfig )
import EnvoyGrpc( runServer )

data Opts = Opts
  { config :: String }

optsParser = O.info
  (O.helper *> v *> p)
  (O.fullDesc <> O.progDesc "Inertia" <> O.header "Inertia - envoy control plane")
  where
    v = O.infoOption (showVersion version) $ mconcat
      [ O.long "version"
      , O.short 'v'
      , O.help "Show version" ]
    p = fmap Opts . O.strOption $ mconcat
      [ O.long "config"
      , O.short 'c'
      , O.metavar "FILEPATH"
      , O.help "Path to config file" ]

main :: IO ()
main = do
  Opts{..} <- O.execParser optsParser
  Config{..} <- fileConfig config
  let AdsConfig{..} = adsConfig
  rules <- traverse listenerRules listeners
  stderrHandler <- fmap
    (flip LH.setFormatter $ LF.simpleLogFormatter "[$time | $prio | $loggername] $msg")
    (LH.streamHandler stderr L.DEBUG)
  _ <- L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG . L.setHandlers [stderrHandler])
  _ <- L.infoM "Main" ("Running ADS on " ++ host ++ " port " ++ show port)
  runServer adsConfig rules destinations
