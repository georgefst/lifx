module Lifx.Program.CmdParser where

import Data.Char
import Data.List
import qualified Data.Set as S
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
  , aTarget :: Targets
  , aCmd :: LiteCmd
  , aHelp :: Maybe (HelpFormat, TextFormat)
  , aDuration :: LiFrac
  } deriving (Show, Eq, Ord)

data LiteCmd = CmdNone
             | CmdList    Int
             | CmdOn
             | CmdOff
             | CmdColor   ColorArg
             | CmdPulse   PulseArg
             | CmdBreathe PulseArg
             | CmdPing
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
                   , aTarget = TargAll
                   , aCmd = CmdNone
                   , aHelp = Just (HelpFormatOne, defaultWrap)
                   , aDuration = 1
                   }

defList    = defNone { aCmd = CmdList 80, aHelp = Nothing }
defOn      = defList { aCmd = CmdOn }
defOff     = defList { aCmd = CmdOff }
defColor   = defList { aCmd = CmdColor   (CNamed White) }
defPulse   = defList { aCmd = CmdPulse   defPulseArg }
defBreathe = defList { aCmd = CmdBreathe defPulseArg }
defPing    = defList { aCmd = CmdPing }

gFlags = iFlag : helpFlag : targFlags

helpFlag = flagHelpFormat $ \hf tf args -> args { aHelp = Just (hf, tf) }

iFlag = flagReq ["I", "interface"] ifaceUpdate "STRING"
        "Name of network interface to use"

ifaceUpdate :: String -> LiteArgs -> Either String LiteArgs
ifaceUpdate arg args = Right $ args { aInterface = Just $ T.pack arg }

widthFlag = flagReq ["w", "width"] updWidth "INTEGER"
            "Width in columns for listing."
  where updWidth arg args = do
          w <- readEither' arg
          return $ args { aCmd = CmdList w }

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

durFlag = flagReq ["d", "duration"] durFlagUpdate "FLOAT"
          "Number of seconds that change should occur over"

durFlagUpdate :: String -> LiteArgs -> Either String LiteArgs
durFlagUpdate arg args = do
  x <- readEither' arg
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
