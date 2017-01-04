data ConnectionArgs = UnspecifiedConnection
                    | LanConnection (Maybe Interface)
                    | CloudConnection (Maybe AccessToken)
                    deriving (Eq, Show, Ord)

data LiteArgs =
  LiteArgs
  { aConnection :: ConnectionArgs
  , aTarget :: Targets
  , aWidth :: Int
  , aCmd :: LiteCmd
  } deriving (Show, Eq, Ord)

data LiteArgs2 =
  LiteArgs2
  { a2Connection :: ConnectionArgs
  , a2Target :: TargetArgs
  , a2Width :: Int
  , a2Cmd :: LiteCmd
  } deriving (Show, Eq, Ord)

data TargetArgs =
  TargetArgs
  { taLabel      :: [T.Text]
  , taId         :: [T.Text]
  , taGroup      :: [T.Text]
  , taGroupId    :: [T.Text]
  , taLocation   :: [T.Text]
  , taLocationId :: [T.Text]
  }

data LiteCmd = CmdList
             | CmdOn       { lcDuration :: FracSeconds }
             | CmdOff      { lcDuration :: FracSeconds }
             | CmdColor    { lcDuration :: FracSeconds, lcColor :: PartialColor }
             | CmdPulse    { lcDuration :: FracSeconds, lcColor :: PartialColor, lcPulse :: PulseArg }
             | CmdBreathe  { lcDuration :: FracSeconds, lcColor :: PartialColor, lcPulse :: PulseArg }
             | CmdSetLabel { lcLabel :: T.Text }
               deriving (Show, Eq, Ord)

data PulseArg =
  PulseArg
  { paPeriod    :: FracSeconds
  , paCycles    :: Double
  , paPersist   :: Bool
  , paPowerOn   :: Bool
  , paPeak      :: Double
  } deriving (Show, Eq, Ord)

connectionArgs :: Parser ConnectionArgs
connectionArgs =
