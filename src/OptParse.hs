{-
    OptsParse module

    One-at-a-time option parser, allowing for modifications to the
    options lookup table between option lookups (i.e., allowing options
    to modify what options are available.
-}

module OptParse
    (
        OptDesc(..),
        ArgDesc(..),
        takesArg,
        OptTable,       -- useful?
        OptParseCtx,    -- but not constructors
        newOptsParseCtx,
        shortOptTable,
        longOptTable,
        getNextOpt,
    ) where

-- public
data OptDesc a =
    ShortOptNoArg Char a
    | LongOptNoArg String a
    | ShortOptArg Char String a
    | LongOptArg String String a
    | RemainingArgs [String]    -- Indicates end of processing.
    -- The rest are error signals
    | ShortOptMissingArg Char
    | LongOptMissingArg String
    | UnknownShortOpt Char
    | UnknownLongOpt String
    deriving (Show)

-- public
data ArgDesc = TakesNoArg | TakesArg deriving (Show)

-- public
takesArg :: ArgDesc -> Bool
takesArg TakesNoArg = False
takesArg TakesArg   = True

type OptTable key payload = [(key, (ArgDesc, payload))]

-- public type, PRIVATE constructor/(some) field accessors
data OptParseCtx a =
    OptParseCtx {
        shortOptTable   :: OptTable Char a,     -- public (for updates)
        longOptTable    :: OptTable String a,   -- public (for updates)
        partialShort    :: String,
        mixedArgs       :: [String],    -- args encountered btwn opts
        remainArgs      :: [String]
    }

-- public OptPraseCtx creator
newOptsParseCtx :: [String] -> OptParseCtx a
newOptsParseCtx args =
    OptParseCtx {
        shortOptTable = [],
        longOptTable  = [],
        partialShort  = "",
        mixedArgs     = [],
        remainArgs    = args
    }

-- public (crucial)
getNextOpt :: OptParseCtx a -> (OptParseCtx a, OptDesc a)

-- Handle any remaining short options from previous arg.
getNextOpt ctx@(OptParseCtx { partialShort = partial@(opt:opts) })
    = case foundOpt of
        Nothing                     -> (ctxNextOpt, UnknownShortOpt opt)
        Just (TakesNoArg, payload)  ->
            (ctxNextOpt, ShortOptNoArg opt payload)
        Just (TakesArg, payload) ->
            case opts of
                [] -> case remainArgs ctx of
                           (arg:args) ->
                               (
                                   ctxNextOpt {remainArgs=args},
                                   ShortOptArg opt arg payload
                               )
                           _ -> (ctxNextOpt, ShortOptMissingArg opt)
                _  -> (
                          ctx { partialShort = [] },
                          ShortOptArg opt opts payload
                      )
  where
    ctxNextOpt = ctx { partialShort = opts }
    foundOpt = lookup opt (shortOptTable ctx)

-- Terminate when "--" arg reached
getNextOpt ctx@(OptParseCtx { remainArgs = ("--":args) })
    = (
        ctx { mixedArgs = [], remainArgs = [] },
        RemainingArgs $ mixedArgs ctx ++ remainArgs ctx
      )
