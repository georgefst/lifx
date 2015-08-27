module Lifx.Program.CmdParser where

import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text (TextFormat(..), showText, defaultWrap)
import qualified System.Console.CmdArgs.Text as TXT (Text(..))
import System.Exit
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import Text.Read

import Lifx.Types


data LiteArgs =
  LiteArgs
  { aInterface :: Maybe Text
  , aTarget :: Selector
  , aCmd :: LiteCmd
  , aHelp :: Maybe (HelpFormat, TextFormat)
  , aDuration :: LiFrac
  } deriving (Show, Eq, Ord)

data LiteCmd = CmdNone
             | CmdList
             | CmdOn
             | CmdOff
             | CmdColor   ColorArg
             | CmdPulse   PulseArg
             | CmdBreathe PulseArg
               deriving (Show, Eq, Ord)

data PulseArg =
  PulseArg
  { paColor     :: ColorArg
  , paFromColor :: ColorArg
  , paPeriod    :: LiFrac
  , paCycles    :: LiFrac
  , paPersist   :: Bool
  , paPowerOn   :: Bool
  , paPeak      :: LiFrac
  } deriving (Show, Eq, Ord)

defPulseArg = PulseArg
  { paColor     = emptyColor
  , paFromColor = emptyColor
  , paPeriod    = 1.0
  , paCycles    = 1.0
  , paPersist   = False
  , paPowerOn   = True
  , paPeak      = 0.5
  }

-- readEither has been in Text.Read since base 4.6,
-- but we have our own copy here to work with base 4.5.
-- BSD3, (c) The University of Glasgow 2001
readEither' :: Read a => String -> Either String a
readEither' s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift skipSpaces
       return x

defNone :: LiteArgs
defNone = LiteArgs { aInterface = Nothing
                   , aTarget = SelAll
                   , aCmd = CmdNone
                   , aHelp = Just (HelpFormatOne, defaultWrap)
                   , aDuration = 1
                   }

defList    = defNone { aCmd = CmdList, aHelp = Nothing }
defOn      = defList { aCmd = CmdOn }
defOff     = defList { aCmd = CmdOff }
defColor   = defList { aCmd = CmdColor   (CNamed White) }
defPulse   = defList { aCmd = CmdPulse   defPulseArg }
defBreathe = defList { aCmd = CmdBreathe defPulseArg }

gFlags = [iFlag, helpFlag]

helpFlag = flagHelpFormat $ \hf tf args -> args { aHelp = Just (hf, tf) }

iFlag = Flag
  { flagNames = ["i", "interface"]
  , flagInfo = FlagReq
  , flagValue = ifaceUpdate
  , flagType = "STRING"
  , flagHelp = "Name of network interface to use"
  }

ifaceUpdate :: String -> LiteArgs -> Either String LiteArgs
ifaceUpdate arg args = Right $ args { aInterface = Just $ T.pack arg }

cFlags = [hFlag, sFlag, bFlag, kFlag, nFlag]

-- TODO: 0-100 instead of 0.0-1.0?

hFlag = mkCFlag "hue"        "0-360"     (\c x -> c { hue = x })
sFlag = mkCFlag "saturation" "0.0-1.0"   (\c x -> c { saturation = x })
bFlag = mkCFlag "brightness" "0.0-1.0"   (\c x -> c { brightness = x })
kFlag = mkCFlag "kelvin"     "2500-9000" (\c x -> c { kelvin = x })

upcase :: String -> String
upcase = map toUpper

downcase :: String -> String
downcase = map toLower

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : downcase xs

mkCFlag :: String -> String -> (MaybeColor -> Maybe LiFrac -> MaybeColor)
           -> Flag LiteArgs
mkCFlag name range f =
  flagReq [[head name], name] (cflagUpdate f) "FLOAT"
  ("Set " ++ name ++ " of light's color (" ++ range ++ ")")

cflagUpdate :: (MaybeColor -> Maybe LiFrac -> MaybeColor)
               -> String
               -> LiteArgs
               -> Either String LiteArgs
cflagUpdate f arg args = do
  num <- readEither' arg
  newCmd <- updColor (`f` Just num) (aCmd args)
  return $ args { aCmd = newCmd }

updColor :: (MaybeColor -> MaybeColor) -> LiteCmd -> Either String LiteCmd
updColor f (CmdColor c) = Right $ CmdColor $ CCustom $ f $ customColor c
updColor f (CmdPulse p) = Right $ CmdPulse $ updPulseColor f p
updColor f (CmdBreathe p) = Right $ CmdPulse $ updPulseColor f p
updColor _ _ = Left "Color arguments not applicable to this command"

updPulseColor :: (MaybeColor -> MaybeColor) -> PulseArg -> PulseArg
updPulseColor f p = p { paColor = CCustom $ f $ customColor $ paColor p }

nFlag = flagReq ["n", "color"] updNamed "COLOR-NAME"
        ("Specify color by name (" ++ nameColors ++ ")")

nameColors = intercalate ", " $ map show colors
  where colors = (enumFromTo minBound maxBound) :: [NamedColor]

updNamed :: String -> LiteArgs -> Either String LiteArgs
updNamed arg args = do
  color <- readEither' $ capitalize arg
  newCmd <- updColorNamed (CNamed color) (aCmd args)
  return $ args { aCmd = newCmd }

updColorNamed :: ColorArg -> LiteCmd -> Either String LiteCmd
updColorNamed ca (CmdColor c) = Right $ CmdColor ca
updColorNamed ca (CmdPulse p) = Right $ CmdPulse $ updPulseColorNamed ca p
updColorNamed ca (CmdBreathe p) = Right $ CmdPulse $ updPulseColorNamed ca p
updColorNamed _ _ = Left "Color arguments not applicable to this command"

updPulseColorNamed :: ColorArg -> PulseArg -> PulseArg
updPulseColorNamed ca p = p { paColor = ca }

pFlags = cFlags ++ [pFlag, cFlag, tFlag, oFlag, eFlag]

pFlag = flagReq ["p", "period"] updPeriod "FLOAT" "Time of one cycle in seconds"
cFlag = flagReq ["c", "cycles"] updCycles "FLOAT" "Number of cycles"
tFlag = flagReq ["t", "persist"] updPersist "BOOL" "Remain with new color if true"
oFlag = flagReq ["o", "poweron"] updPowerOn "BOOL" "Power light on if currently off"
eFlag = flagReq ["e", "peak"] updPeak "FLOAT" "Is this different than duty cycle?"

updPeriod = updFrac (\n p -> p { paPeriod = n })
updCycles = updFrac (\n p -> p { paCycles = n })
updPersist = updBool (\b p -> p { paPersist = b })
updPowerOn = updBool (\b p -> p { paPowerOn = b })
updPeak = updFrac (\n p -> p { paPeak = n })

updFrac :: (LiFrac -> PulseArg -> PulseArg)
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
  x <- readEither' (f1 arg)
  newCmd <- updPulse2 (f2 x) (aCmd args)
  return $ args { aCmd = newCmd }

updPulse2 :: (PulseArg -> PulseArg) -> LiteCmd -> Either String LiteCmd
updPulse2 f (CmdPulse p) = Right $ CmdPulse (f p)
updPulse2 f (CmdBreathe p) = Right $ CmdBreathe (f p)
updPulse2 _ _ = Left "Pulse arguments not applicable to this command"

updArg :: String -> LiteArgs -> Either String LiteArgs
updArg arg args = Right $ args { aTarget = sel arg }
  where sel "all" = SelAll
        sel label = SelLabel $ T.pack label

selArg = Arg
  { argValue = updArg
  , argType = "[LABEL|DEVID]"
  , argRequire = False
  }

durFlag = flagReq ["d", "duration"] durFlagUpdate "FLOAT"
          "Number of seconds that change should occur over"

durFlagUpdate :: String -> LiteArgs -> Either String LiteArgs
durFlagUpdate arg args = do
  x <- readEither' arg
  return $ args { aDuration = x }

arguments :: Mode LiteArgs
arguments =
  (modes  "lifx"    defNone  "Control LIFX light bulbs"
   [ mode "list"    defList  "List bulbs"        selArg gFlags
   , mode "on"      defOn    "Turn bulb on"      selArg (durFlag : gFlags)
   , mode "off"     defOff   "Turn bulb off"     selArg (durFlag : gFlags)
   , mode "color"   defColor "Set bulb color"    selArg (durFlag : cFlags ++ gFlags)
   , mode "pulse"   defPulse "Square wave blink" selArg (pFlags ++ gFlags)
   , mode "breathe" defPulse "Sine wave blink"   selArg (pFlags ++ gFlags)
   ]) { modeGroupFlags = toGroup gFlags }

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
