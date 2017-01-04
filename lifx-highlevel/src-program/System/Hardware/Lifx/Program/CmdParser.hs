module System.Hardware.Lifx.Program.CmdParser where

import Data.Char
import Data.List
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text (TextFormat(..), showText, defaultWrap)
import qualified System.Console.CmdArgs.Text as TXT (Text(..))
import System.Exit
import Text.Read (readEither)

import System.Hardware.Lifx
import System.Hardware.Lifx.Program.TargetMatch

data LiteArgs =
  LiteArgs
  { aInterface :: Maybe Interface
  , aTarget :: Targets
  , aCmd :: LiteCmd
  , aHelp :: Maybe (HelpFormat, TextFormat)
  , aDuration :: FracSeconds
  } deriving (Show, Eq, Ord)

data LiteCmd = CmdNone
             | CmdList    !Int
             | CmdOn
             | CmdOff
             | CmdColor   { lcColor :: PartialColor }
             | CmdPulse   { lcColor :: PartialColor, lcPulse :: PulseArg }
             | CmdBreathe { lcColor :: PartialColor, lcPulse :: PulseArg }
             | CmdPing
             | CmdSetLabel Text
               deriving (Show, Eq, Ord)

data PulseArg =
  PulseArg
  { paPeriod    :: FracSeconds
  , paCycles    :: Double
  , paPersist   :: Bool
  , paPowerOn   :: Bool
  , paPeak      :: Double
  } deriving (Show, Eq, Ord)

defPulseArg = PulseArg
  { paPeriod    = 1.0
  , paCycles    = 1.0
  , paPersist   = False
  , paPowerOn   = True
  , paPeak      = 0.5
  }

downcase :: String -> String
downcase = map toLower

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : downcase xs

defNone :: LiteArgs
defNone = LiteArgs { aInterface = Nothing
                   , aTarget = TargAll
                   , aCmd = CmdNone
                   , aHelp = Just (HelpFormatOne, defaultWrap)
                   , aDuration = 1
                   }

defList    = defNone { aCmd = CmdList 80, aHelp = Nothing }
defOn      = defList { aCmd = CmdOn }
defOff     = defList { aCmd = CmdOff }
defColor   = defList { aCmd = CmdColor   emptyColor }
defPulse   = defList { aCmd = CmdPulse   emptyColor defPulseArg }
defBreathe = defList { aCmd = CmdBreathe emptyColor defPulseArg }
defPing    = defList { aCmd = CmdPing }
defSetLabel =
  defList { aCmd = CmdSetLabel
                   $ error "Use the -L argument to set the new label" }

gFlags = iFlag : helpFlag : targFlags

helpFlag = flagHelpFormat $ \hf tf args -> args { aHelp = Just (hf, tf) }

iFlag = flagReq ["I", "interface"] ifaceUpdate "STRING"
        "Name of network interface to use"
  where
    ifaceUpdate arg args = Right $ args { aInterface = Just $ T.pack arg }

widthFlag = flagReq ["w", "width"] updWidth "INTEGER"
            "Width in columns for listing."
  where updWidth arg args = do
          w <- readEither arg
          return $ args { aCmd = CmdList w }

labFlag = flagReq ["L", "new-label"] updLabel "STRING" "New label."
  where updLabel arg args =
          return $ args { aCmd = CmdSetLabel $ T.pack arg }

cFlags =
  [ mkCFlag "hue"        "0-360" (\c x -> c { hue = Just x })
  , mkCFlag "saturation" "0-100" (\c x -> c { saturation = Just $ x / 100 })
  , mkCFlag "brightness" "0-100" (\c x -> c { brightness = Just $ x / 100 })
  , mkCFlag "kelvin"     "2500-9000" (\c x -> c { kelvin = Just x })
  ]

mkCFlag :: String -> String -> (PartialColor -> ColorChannel -> PartialColor)
           -> Flag LiteArgs
mkCFlag name range f =
  flagReq [[head name], name] cflagUpdate "FLOAT"
  ("Set light's " ++ whatIs name ++ " (" ++ range ++ ")")
  where whatIs "kelvin" = "color temperature"
        whatIs x = x
        updColor f' lc = lc { lcColor = f' $ lcColor lc }
        cflagUpdate arg args = do
          num <- readEither arg
          let newCmd = updColor (`f` num) (aCmd args)
          return $ args { aCmd = newCmd }

pFlags =
  cFlags ++
  [ flagReq ["p", "period"] updPeriod "FLOAT" "Time of one cycle in seconds"
  , flagReq ["c", "cycles"] updCycles "FLOAT" "Number of cycles"
  , flagReq ["t", "persist"] updPersist "BOOL" "Remain with new color if true"
  , flagReq ["o", "poweron"] updPowerOn "BOOL" "Power light on if currently off"
  , flagReq ["e", "peak"] updPeak "FLOAT" "Is this different than duty cycle?"
  ]
  where
    updPeriod = updFrac (\n p -> p { paPeriod = n })
    updCycles = updFrac (\n p -> p { paCycles = n })
    updPersist = updBool (\b p -> p { paPersist = b })
    updPowerOn = updBool (\b p -> p { paPowerOn = b })
    updPeak = updFrac (\n p -> p { paPeak = n })

updFrac :: (ColorChannel -> PulseArg -> PulseArg)
           -> String
           -> LiteArgs
           -> Either String LiteArgs
updFrac = updPulse id

updBool :: (Bool -> PulseArg -> PulseArg)
           -> String
           -> LiteArgs
           -> Either String LiteArgs
updBool = updPulse capitalize

updPulse :: Read a
            => (String -> String)
            -> (a -> PulseArg -> PulseArg)
            -> String
            -> LiteArgs
            -> Either String LiteArgs
updPulse f1 f2 arg args = do
  x <- readEither (f1 arg)
  let updPulse2 f lc = lc { lcPulse = f $ lcPulse lc }
      newCmd = updPulse2 (f2 x) (aCmd args)
  return $ args { aCmd = newCmd }

durFlag = flagReq ["d", "duration"] durFlagUpdate "FLOAT"
          "Number of seconds that change should occur over"
  where
    durFlagUpdate arg args = do
      x <- readEither arg
      return $ args { aDuration = x }

targFlags =
  [ flagReq ["l", "label"] updLabel "STRING" (helpLab "Label" "Left Lamp")
  , flagReq ["i", "id"] updDevId "HEX-STRING" (helpId "Device" "d3b2f2d97452")
  , flagReq ["g", "group"] updGroup "STRING" (helpLab "Group" "Lounge")
  , flagReq ["G", "group-id"] updGroupId "HEX-STRING"
    (helpId "Group" "1c8de82b81f445e7cfaafae49b259c71")
  , flagReq ["a", "location"] updLocation "STRING" (helpLab "Location" "Home")
  , flagReq ["A", "location-id"] updLocationId "HEX-STRING"
    (helpId "Location" "1d6fe8ef0fde4c6d77b0012dc736662c")
  ]
  where updLabel      = updTM TmLabel
        updDevId      = updTM TmDevId
        updGroup      = updTM TmGroup
        updGroupId    = updTM TmGroupId
        updLocation   = updTM TmLocation
        updLocationId = updTM TmLocationId

        updTM con arg args =
          Right $ args { aTarget = updTM' (aTarget args) (con $ T.pack arg) }
        updTM' TargAll tm = TargSome $ S.singleton tm
        updTM' (TargSome s) tm = TargSome $ S.insert tm s

        helpLab name example = concat
          [ name
          , " name.  May be abbreviated as a unique prefix of the "
          , downcase name
          , ".  For example, \""
          , take (length example `div` 2) example
          , "\" instead of \""
          , example
          , "\"."
          ]

        helpId name example = concat
          [ name
          , " ID.  May be abbreviated as a unique suffix of the"
          , " ID.  For example, \""
          , drop (length example - 4) example
          , "\" instead of \""
          , example
          , "\"."
          ]

arguments :: Mode LiteArgs
arguments =
  (modes  "lifx"    defNone  "Control LIFX light bulbs"
   [ myMode "list"    defList  "List bulbs"         (widthFlag : gFlags)
   , myMode "on"      defOn    "Turn bulb on"       (durFlag : gFlags)
   , myMode "off"     defOff   "Turn bulb off"      (durFlag : gFlags)
   , myMode "color"   defColor "Set bulb color"     (durFlag : cFlags ++ gFlags)
   , myMode "pulse"   defPulse "Square wave blink"  (pFlags ++ gFlags)
   , myMode "breathe" defPulse "Sine wave blink"    (pFlags ++ gFlags)
   , myMode "ping"    defPing  "Check connectivity" gFlags
   , myMode "set-label" defSetLabel "Set bulb's label" (labFlag : gFlags)
   ]) { modeGroupFlags = toGroup gFlags }
  where myMode name value help flags =
          (mode name value help undefined flags) { modeArgs=([], Nothing) }

handleHelp :: Maybe (HelpFormat, TextFormat) -> IO ()
handleHelp Nothing = return ()
handleHelp (Just (hf, tf)) = do
  let txt = helpText [] hf arguments
      extra = [ TXT.Line ""
              , TXT.Line "For more detailed help, do \"--help=all\"" ]
      ext HelpFormatDefault = extra
      ext HelpFormatOne = extra
      ext _ = []
      str = showText tf (txt ++ ext hf)
  putStr str
  exitSuccess

parseCmdLine :: IO LiteArgs
parseCmdLine = do
  args <- processArgs arguments
  handleHelp $ aHelp args
  return args
