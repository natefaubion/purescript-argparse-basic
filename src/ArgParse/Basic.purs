module ArgParse.Basic
  ( ArgParser
  , ArgError(..)
  , ArgErrorMsg(..)
  , ArgHelp(..)
  , flag
  , flagHelp
  , flagInfo
  , argument
  , fromRecord
  , unformat
  , int
  , number
  , boolean
  , separated
  , many
  , many1
  , unfolded
  , unfolded1
  , folded
  , folded1
  , default
  , optional
  , choose
  , command
  , any
  , anyNotFlag
  , rest
  , parseArgs
  , printArgError
  , printHelp
  , parserHelp
  , class BuildRecordArgs
  , buildRecordArgs
  ) where

import Prelude

import Control.Alternative (guard, (<|>))
import Data.Array (elem)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Foldable (class Foldable, fold)
import Data.Function (on)
import Data.Int as Int
import Data.List (List(..), foldr, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.Ord (abs)
import Data.Semigroup.Foldable (fold1)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr1)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

type PrimArgParser a = List String -> ArgResult a

flag' :: Array String -> PrimArgParser Unit
flag' names = go
  where
  go = case _ of
    arg : args | arg `elem` names ->
      ArgMatch unit args
    _ ->
      ArgFail

argument' :: List ArgHelp -> Array String -> PrimArgParser String
argument' stk names = go
  where
  matchParam arg =
    Array.findMap (flip String.stripPrefix arg <<< Pattern) names

  go = case _ of
    arg : args | Just value <- matchParam arg ->
      if SCU.take 1 value == "=" then
        ArgMatch (SCU.drop 1 value) args
      else if value == "" then
        case args of
          value' : args' ->
            ArgMatch value' args'
          _ ->
            ArgHalt (ArgError stk ExpectedArgValue)
      else
        ArgFail
    _ ->
      ArgFail

data ArgHelp
  = HelpFlag (Array String) String
  | HelpAny String
  | HelpFormat String ArgHelp
  | HelpArgs (Array ArgHelp)
  | HelpChoose String (Array ArgHelp)
  | HelpCommand (Array String) String ArgHelp
  | HelpRest String

data ArgError = ArgError (List ArgHelp) ArgErrorMsg

data ArgErrorMsg
  = ExpectedFlag
  | ExpectedArgValue
  | ExpectedRest
  | ExpectedArg
  | DuplicateArg
  | UnformatFailed String
  | UnknownArg String
  | ShowHelp
  | ShowInfo String

data ArgResult a
  = ArgHalt ArgError
  | ArgMatch a (List String)
  | ArgFail

derive instance functorArgResult :: Functor ArgResult

data ArgParser a = ArgParser ArgHelp (ArgFold a)

derive instance functorArgParser :: Functor ArgParser

instance applyParser :: Apply ArgParser where
  apply (ArgParser h1 m1) (ArgParser h2 m2) =
    ArgParser (h1 <> h2) (m1 <*> m2)

instance semigroupHelp :: Semigroup ArgHelp where
  append = case _, _ of
    HelpArgs hs1, HelpArgs hs2 -> HelpArgs (hs1 <> hs2)
    HelpArgs hs1, b -> HelpArgs (Array.snoc hs1 b)
    a, HelpArgs hs2 -> HelpArgs (Array.cons a hs2)
    a, b -> HelpArgs [ a, b ]

newtype ArgFold a =
  ArgFold
    { step :: List ArgHelp -> List String -> ArgResult (ArgFold a)
    , done :: List ArgHelp -> Either ArgError a
    , saturated :: Boolean
    }

derive instance functorArgFold :: Functor ArgFold

instance applyArgFold :: Apply ArgFold where
  apply (ArgFold f1) (ArgFold f2) = ArgFold { step, done, saturated: f1.saturated && f2.saturated }
    where
    step stk args =
      case f1.step stk args of
        ArgHalt err -> ArgHalt err
        ArgMatch next args' ->
          ArgMatch (next <*> ArgFold f2) args'
        ArgFail ->
          case f2.step stk args of
            ArgHalt err ->
              ArgHalt err
            ArgMatch next args' ->
              ArgMatch (ArgFold f1 <*> next) args'
            ArgFail ->
              ArgFail
    done stk =
      f1.done stk <*> f2.done stk

instance applicativeArgFold :: Applicative ArgFold where
  pure value =
    ArgFold
      { step: \_ _ -> ArgFail
      , done: const (Right value)
      , saturated: true
      }

fail :: forall a. ArgError -> ArgFold a
fail err = ArgFold
  { step: \_ _ -> ArgHalt err
  , done: const (Left err)
  , saturated: true
  }

failDup :: forall a. ArgHelp -> ArgErrorMsg -> ArgFold a -> ArgFold a
failDup help errMsg (ArgFold f@{ done }) = ArgFold { step, done, saturated: true }
  where
  step stk args =
    case f.step stk args of
      ArgHalt err -> ArgHalt err
      ArgFail -> ArgFail
      ArgMatch _ _ -> ArgHalt (ArgError (help : stk) errMsg)

-- | Matches a single flag. For flags that return `Boolean`, combine
-- | with `boolean`.
-- |
-- | ```purescript
-- | example =
-- |   flag [ "--toggle", "-t" ] "Toggles something."
-- |     # boolean
-- | ```
flag :: Array String -> String -> ArgParser Unit
flag name doc = ArgParser help $ ArgFold { step, done, saturated: false }
  where
  help = HelpFlag name doc
  step _ = flag' name >>> map \value ->
    failDup help DuplicateArg $ ArgFold { step, done: const (Right value), saturated: true }
  done stk =
    Left (ArgError (help : stk) ExpectedFlag)

-- | Matches a flag, followed by a single argument, yielding the argument.
-- | The argument and flag may be separated by either a space or `=`.
-- |
-- | ```purescript
-- | example = argument [ "--arg", "-a" ] "Some argument."
-- | ```
-- |
-- | In this example, all of the following would be valid ways to pass in an
-- | argument:
-- |
-- | * `--arg value`
-- | * `--arg=value`
-- | * `-a value`
-- | * `-a=value`
argument :: Array String -> String -> ArgParser String
argument name doc = ArgParser help $ ArgFold { step, done, saturated: false }
  where
  help = HelpFlag name doc
  step stk = argument' (help : stk) name >>> map \value ->
    failDup help DuplicateArg $ ArgFold { step, done: const (Right value), saturated: true }
  done stk =
    Left (ArgError (help : stk) ExpectedArg)

-- Not currently exported.
partial :: forall a b. (a -> Either ArgErrorMsg b) -> ArgParser a -> ArgParser b
partial unf (ArgParser help init) = ArgParser help (go init)
  where
  go (ArgFold f) = ArgFold { step, done, saturated: false }
    where
    step stk args =
      go <$> f.step stk args
    done stk =
      lmap (ArgError (help : stk)) <<< unf =<< f.done stk

-- | Parses a value into some other type, or fails with an error.
-- |
-- | ```purescript
-- | example =
-- |   argument [ "--date", "-d" ] "Some date."
-- |     # unformat "DATE" (unformatDateTime "YYYY-MM-DD")
-- |     -- From purescript-formatters
-- | ```
unformat :: forall a b. String -> (a -> Either String b) -> ArgParser a -> ArgParser b
unformat doc unf (ArgParser h init) = ArgParser help $ go init
  where
  help = HelpFormat doc h
  go (ArgFold f@{ saturated }) = ArgFold { step, done, saturated }
    where
    step stk args =
      case f.step stk args of
        ArgHalt (ArgError (_ : stk') err) ->
          ArgHalt (ArgError (help : stk') err)
        ArgHalt err -> ArgHalt err
        ArgMatch a b -> ArgMatch (go a) b
        ArgFail -> ArgFail
    done stk = do
      value <- f.done (help : stk)
      lmap (ArgError (help : stk) <<< UnformatFailed) $ unf value

-- | Parses an argument as an `Int`.
-- |
-- | ```purescript
-- | example =
-- |   argument [ "--int", "-i" ] "Some integer."
-- |     # int
-- | ```
int :: ArgParser String -> ArgParser Int
int = unformat "INTEGER" (note "Invalid integer." <<< Int.fromString)

-- | Parses an argument as an `Number`.
-- |
-- | ```purescript
-- | example =
-- |   argument [ "--num", "-n" ] "Some number."
-- |     # number
-- | ```
number :: ArgParser String -> ArgParser Number
number = unformat "NUMBER" (note "Invalid number." <<< Number.fromString)

-- | Parses a flag as a `Boolean`, defaulting to `false`.
-- |
-- | ```purescript
-- | example =
-- |   flag [ "--flag", "-f" ] "Some boolean."
-- |     # boolean
-- | ```
boolean :: ArgParser Unit -> ArgParser Boolean
boolean = default false <<< (true <$ _)

-- | Splits a value on some separator (using `Data.String.split`).
-- |
-- | ```purescript
-- | example =
-- |   argument [ "--sep", "-s" ] "Separated by commas"
-- |     # separated "ARG" (Pattern ",")
-- | ```
separated :: String -> Pattern -> ArgParser String -> ArgParser (Array String)
separated doc pat = unformat ("[" <> doc <> unwrap pat <> "...]") (Right <<< String.split pat)

-- | Collects multiple matches of a parser into a `List`.
-- |
-- | ```purescript
-- | example =
-- |   argument [ "--many", "-m" ] "Collect multiple arguments"
-- |     # many
-- | ```
-- |
-- | In this example, passing int `--many a --many b -m c` would yield
-- | a `List` with three values.
many :: forall a. ArgParser a -> ArgParser (List a)
many (ArgParser help f) = ArgParser help (go1 Nil false f)
  where
  go1 acc parsing (ArgFold { step, done }) =
    ArgFold
      { step: \stk -> map (go2 acc stk) <<< step stk
      , done: \stk ->
          if parsing then do
            next <- done stk
            Right (List.reverse (next : acc))
          else
            Right (List.reverse acc)
      , saturated: false
      }

  go2 acc stk f'@(ArgFold { done, saturated }) =
    if saturated then
      case done stk of
        Left err ->
          fail err
        Right a ->
          go1 (a : acc) false f
    else
      go1 acc true f'

-- | Collects multiple matches of a parser into a `NonEmptyList`. Omitting
-- | the argument yields an error.
many1 :: forall a. ArgParser a -> ArgParser (NonEmptyList a)
many1 = partial go <<< many
  where
  go values =
    case NonEmptyList.fromList values of
      Nothing ->
        Left ExpectedArg
      Just values' ->
        Right values'

-- | Similar to `many`, but collects matches into any `Unfoldable`.
unfolded :: forall f a. Unfoldable f => ArgParser a -> ArgParser (f a)
unfolded = map List.toUnfoldable <<< many

-- | Similar to `many1`, but collects matches into any `Unfoldable1`.
unfolded1 :: forall f a. Unfoldable1 f => ArgParser a -> ArgParser (f a)
unfolded1 = map (unfoldr1 go) <<< many1
  where
  go nel = Tuple (NonEmptyList.head nel) (NonEmptyList.fromList (NonEmptyList.tail nel))

-- | Similar to `many`, but collects matches with `Monoid`.
folded :: forall a. Monoid a => ArgParser a -> ArgParser a
folded = map fold <<< many

-- | Similar to `many1`, but collects matches with `Semigroup`.
folded1 :: forall a. Semigroup a => ArgParser a -> ArgParser a
folded1 = map fold1 <<< many1

-- | Provides a default value for when an argument is not otherwise provided.
default :: forall a. a -> ArgParser a -> ArgParser a
default value (ArgParser help (ArgFold { step, done })) =
  ArgParser help $ ArgFold
    { step
    , done: either (const (Right value)) Right <<< done
    , saturated: true
    }

-- | Makes any argument optional with `Maybe`.
optional :: forall a. ArgParser a -> ArgParser (Maybe a)
optional = default Nothing <<< map Just

-- | Matches one value from a set of parsers. This is useful for commmands
-- | and enumerations.
-- |
-- | ```purescript
-- | data Order = First | Second | Third
-- |
-- | example = choose "order"
-- |   [ flag [ "--first", "-1" ] "First in order." $> First
-- |   , flag [ "--second", "-2" ] "Second in order." $> Second
-- |   [ flag [ "--third", "-3" ] "Third in order." $> Third
-- |   ]
-- | ```
choose :: forall a. String -> Array (ArgParser a) -> ArgParser a
choose name parsers =
  ArgParser help $ ArgFold
    { step: go1 (List.fromFoldable (map (\(ArgParser _ am) -> am) parsers))
    , done: \stk -> Left (ArgError (help : stk) ExpectedArg)
    , saturated: false
    }
  where
  help = HelpChoose name $ parserHelp <$> parsers

  go1 parsers' stk args =
    go2 stk args Nil parsers'

  go2 stk args acc = case _ of
    ArgFold am : ams ->
      case am.step stk args of
        ArgHalt err -> ArgHalt err
        ArgFail ->
          go2 stk args (ArgFold am : acc) ams
        ArgMatch (ArgFold next) args' ->
          if next.saturated then
            ArgMatch
              ( failDup help DuplicateArg $ ArgFold
                  { step: go1 (foldr (:) (ArgFold next : ams) acc)
                  , done: next.done
                  , saturated: true
                  }
              )
              args'
          else
            ArgMatch
              ( ArgFold
                  { step: \stk' args'' -> go2 stk' args'' acc (ArgFold next : ams)
                  , done: next.done
                  , saturated: false
                  }
              )
              args'
    Nil ->
      ArgFail

-- | Adds a `--help` flag to a parser, which will immediately halt the parser
-- | and display help documentation. Best combined with `*>` or `<*`.
-- |
-- | ```purescript
-- | example =
-- |   flagHelp *> fromRecord
-- |     { ...
-- |     }
-- | ```
flagHelp :: ArgParser Unit
flagHelp = ArgParser help (ArgFold { step, done, saturated: true })
  where
  name = [ "--help", "-h" ]
  help = HelpFlag name "Show this help message."
  step stk args =
    case flag' name args of
      ArgHalt err -> ArgHalt err
      ArgFail -> ArgFail
      ArgMatch _ _ -> ArgHalt (ArgError stk ShowHelp)
  done =
    const (Right unit)

-- | Adds an info flag to a parser, which will immediately halt the parser
-- | and display the information. Useful for version flags. Best combined with
-- | `*>` or `<*`.
-- |
-- | ```purescript
-- | example =
-- |   flagInfo [ "--version", "-v" ] "1.0.0"
-- |     *> fromRecord { ... }
-- | ```
flagInfo :: Array String -> String -> String -> ArgParser Unit
flagInfo name doc info = ArgParser help (ArgFold { step, done, saturated: true })
  where
  help = HelpFlag name doc
  step stk args =
    case flag' name args of
      ArgHalt err -> ArgHalt err
      ArgFail -> ArgFail
      ArgMatch _ _ -> ArgHalt (ArgError (help : stk) (ShowInfo info))
  done =
    const (Right unit)

-- | Builds a record parser from a record of parsers.
-- |
-- | ```purescript
-- | example = fromRecord
-- |   { int:
-- |       argument [ "--int", "-i" ] "Some integer."
-- |         # int
-- |   , date:
-- |       argument [ "--date", "-d" ] " Some date."
-- |         # unformat "DATE" (unformatDateTime "YYYY-MM-DD")
-- |   , many:
-- |       argument [ "--many", "-m" ] "Collect multiple arguments"
-- |         # many
-- |   }
-- | ```
fromRecord
  :: forall rin rl rout
   . RowToList rin rl
  => BuildRecordArgs rl { | rin } {} { | rout }
  => { | rin }
  -> ArgParser { | rout }
fromRecord =
  map Builder.buildFromScratch
    <<< buildRecordArgs (Proxy :: _ rl)

class BuildRecordArgs (rl :: RowList Type) rs rin rout | rl -> rout where
  buildRecordArgs :: Proxy rl -> rs -> ArgParser (Builder rin rout)

instance buildRecordArgsNil :: BuildRecordArgs RowList.Nil rs rout rout where
  buildRecordArgs _ _ = ArgParser (HelpArgs []) $ pure identity

instance buildArgsCons ::
  ( IsSymbol sym
  , Row.Cons sym (ArgParser a) rs' rs
  , Row.Cons sym a rin rin'
  , Row.Lacks sym rin
  , BuildRecordArgs next { | rs } { | rin' } { | rout }
  ) =>
  BuildRecordArgs (RowList.Cons sym (ArgParser a) next) { | rs } { | rin } { | rout } where
  buildRecordArgs _ rs =
    (\a b -> Builder.insert (Proxy :: _ sym) a >>> b)
      <$> Record.get (Proxy :: _ sym) rs
      <*> buildRecordArgs (Proxy :: _ next) rs

-- | Runs a parser as a sub-command, where it is expected to consume the rest
-- | of the argument input. Best combined with `choose` for multiple commands.
-- |
-- | ```purescript
-- | example =
-- |   command [ "some-command", "sc" ] "Some command." do
-- |     flagHelp *> fromRecord
-- |       { ...
-- |       }
-- | ```
command :: forall a. Array String -> String -> ArgParser a -> ArgParser a
command name doc (ArgParser h m) = ArgParser help $ go m
  where
  help = HelpCommand name doc h
  go f@(ArgFold { done }) = ArgFold { step, done, saturated: false }
    where
    step stk args =
      case flag' name args of
        ArgHalt err -> ArgHalt err
        ArgFail -> ArgFail
        ArgMatch _ args' ->
          case parseArgs' f (help : stk) args' of
            Left err ->
              ArgHalt err
            Right value ->
              ArgMatch (pure value) Nil

-- | Matches any argument that satisfies a given predicate. Good for collecting
-- | leftover arguments. It is not recommended to use `any` with `fromRecord`,
-- | but instead order it specifically after other parsers using Applicative
-- | combinators.
-- |
-- | ```purescript
-- | example =
-- |   Tuple
-- |     <$> fromRecord { ... }
-- |     <*> any "ANY" "Any leftover arguments." Just
-- | ```
any :: forall a. String -> String -> (String -> Maybe a) -> ArgParser a
any name doc k = ArgParser help $ ArgFold { step, done, saturated: false }
  where
  help = HelpFormat name (HelpAny doc)
  step _ = case _ of
    a : args | Just value <- k a ->
      ArgMatch (pure value) args
    _ ->
      ArgFail
  done stk =
    Left (ArgError (help : stk) ExpectedArg)

-- | Like `any`, but matches anything that does not start with a "-". This
-- | works better with fromRecord since it generally does not overlap with other
-- | arguments.
anyNotFlag :: String -> String -> ArgParser String
anyNotFlag name doc = any name doc \str -> guard (String.take 1 str /= "-") $> str

-- | Collects the rest of the arguments in an input stream after encountering
-- | a `--`.
rest :: forall f. Unfoldable f => String -> ArgParser (f String)
rest doc = ArgParser help $ ArgFold { step, done, saturated: false }
  where
  help = HelpRest doc
  step _ = case _ of
    "--" : args ->
      ArgMatch (pure (List.toUnfoldable args)) Nil
    _ ->
      ArgFail
  done stk =
    Left (ArgError (help : stk) ExpectedRest)

-- | Parses an input stream of arguments given a command name and description.
-- | The name and description will be used to generate help.
-- |
-- | ```purescript
-- | example =
-- |   parseArgs "my-cli" "This is my CLI." myParser
-- |     [ "-n", "42" ]
-- | ```
parseArgs :: forall f a. Foldable f => String -> String -> ArgParser a -> f String -> Either ArgError a
parseArgs cmd doc (ArgParser h m) = parseArgs' m (pure (HelpCommand [ cmd ] doc h)) <<< List.fromFoldable

parseArgs' :: forall a. ArgFold a -> List ArgHelp -> List String -> Either ArgError a
parseArgs' (ArgFold { step, done }) stk args =
  case step stk args of
    ArgHalt err ->
      Left err
    ArgMatch next args' ->
      parseArgs' next stk args'
    ArgFail ->
      case args of
        Nil ->
          done stk
        arg : _ ->
          Left $ ArgError stk (UnknownArg arg)

-- | Prints an error to a String. This will include the full error context
-- | and help.
printArgError :: ArgError -> String
printArgError (ArgError stk msg) =
  renderDoc case msg of
    ExpectedFlag ->
      printArgError' $ Text "Expected flag."
    ExpectedArgValue ->
      case stk of
        HelpFormat fmt _ : _ ->
          printArgError' $ Text $ "Expected " <> fmt <> "."
        _ ->
          printArgError' $ Text "Expected argument value."
    ExpectedArg ->
      case stk of
        HelpFormat fmt _ : _ ->
          printArgError' $ Text $ "Expected " <> fmt <> "."
        HelpChoose fmt _ : _ ->
          printArgError' $ Text $ "Expected " <> fmt <> "."
        _ ->
          printArgError' $ Text "Expected argument."
    ExpectedRest ->
      printArgError' $ Text "Expected rest arguments."
    DuplicateArg ->
      case stk of
        HelpChoose fmt _ : _ ->
          printArgError' $ Text $ "Duplicate " <> fmt <> "."
        _ ->
          printArgError' $ Text "Duplicate argument."
    UnformatFailed err ->
      printArgError' $ Text err
    ShowHelp ->
      printArgError' $ Lines []
    ShowInfo info ->
      Text info
    UnknownArg arg ->
      printArgError' $ Lines
        [ Text "Unexpected argument:"
        , Indent $ Text arg
        ]

  where
  getCmd cmd desc help = case _ of
    Nil -> { cmd, desc, help }
    HelpCommand name d h : hs ->
      getCmd (name : cmd) (desc <|> Just d) (help <|> Just h) hs
    HelpFlag name h : hs ->
      getCmd (name : cmd) Nothing (Just (HelpFlag name h)) hs
    HelpAny doc : hs ->
      getCmd cmd Nothing (Just (HelpAny doc)) hs
    HelpFormat _ h : hs ->
      getCmd cmd desc help (h : hs)
    _ : hs ->
      getCmd cmd desc help hs

  printArgError' err = do
    let
      { cmd, desc, help } =
        getCmd Nil Nothing Nothing stk
    Lines
      [ Text $ List.intercalate " " $ Array.intercalate "," <$> cmd
      , Indent $ Paras
          [ err
          , maybe (Lines []) Text desc
          , maybe (Lines []) (printHelp' stk) help
          ]
      ]

-- | Gets the help associated with a parser.
parserHelp :: forall a. ArgParser a -> ArgHelp
parserHelp (ArgParser help _) = help

-- | Prints help to a string.
printHelp :: ArgHelp -> String
printHelp = renderDoc <<< printHelp' Nil

printHelp' :: List ArgHelp -> ArgHelp -> HelpFmt
printHelp' stk = case _ of
  HelpFormat doc next ->
    printHelp' (HelpFormat doc next : stk) next
  HelpCommand name doc opts ->
    Paras
      [ Lines
          [ Text (Array.intercalate "," name)
          , Indent (Text doc)
          ]
      , Indent $ printHelp' stk opts
      ]
  other ->
    Table $ printHelpTable stk other

data HelpClass
  = IsFlag (Array String)
  | IsAny String
  | IsCommand (Array String)

derive instance eqHelpClass :: Eq HelpClass
derive instance ordHelpClass :: Ord HelpClass

printHelpTable :: List ArgHelp -> ArgHelp -> Array (Array HelpFmt)
printHelpTable stk =
  printHelpTable' stk
    >>> Array.sortBy (comparing fst)
    >>> Array.groupBy (groupFn `on` fst)
    >>> map (map snd <<< NonEmptyArray.toArray)
    >>> Array.intercalate [ [ Text "" ] ]

  where
  groupFn = case _, _ of
    IsFlag _, IsFlag _ -> true
    IsAny _, IsAny _ -> true
    IsCommand _, IsCommand _ -> true
    _, _ -> false

printHelpTable' :: List ArgHelp -> ArgHelp -> Array (Tuple HelpClass (Array HelpFmt))
printHelpTable' stk = case _ of
  HelpFlag name doc -> do
    let names = Array.intercalate "," name
    case stk of
      HelpFormat help _ : _ ->
        [ Tuple (IsFlag name) [ Text (names <> " " <> help), Text doc ] ]
      _ ->
        [ Tuple (IsFlag name) [ Text names, Text doc ] ]
  HelpAny doc ->
    case stk of
      HelpFormat help _ : _ ->
        [ Tuple (IsAny help) [ Text help, Text doc ] ]
      _ ->
        [ Tuple (IsAny "ANY") [ Text "ANY", Text doc ] ]
  HelpRest doc ->
    [ Tuple (IsAny "--") [ Text "--", Text doc ] ]
  HelpCommand name doc _ -> do
    [ Tuple (IsCommand name) [ Text (Array.intercalate "," name), Text doc ] ]
  HelpFormat doc next ->
    printHelpTable' (HelpFormat doc next : stk) next
  HelpChoose _ opts ->
    printHelpTable' stk =<< opts
  HelpArgs opts ->
    printHelpTable' stk =<< opts

data HelpFmt
  = Paras (Array HelpFmt)
  | Lines (Array HelpFmt)
  | Table (Array (Array HelpFmt))
  | Indent HelpFmt
  | Text String

renderDoc :: HelpFmt -> String
renderDoc = Array.intercalate "\n" <<< renderDocLines ""

renderDocLines :: String -> HelpFmt -> Array String
renderDocLines ind = case _ of
  Paras docs ->
    join $ Array.intersperse [ "" ] $ Array.filter (not <<< Array.null) $ renderDocLines ind <$> docs
  Lines docs ->
    renderDocLines ind =<< docs
  Text str ->
    map (append ind) $ String.split (Pattern "\n") str
  Indent doc ->
    renderDocLines (ind <> "    ") doc
  Table table ->
    printTableLines ind table

joinColumns :: Int -> String -> Array String -> Array String -> Array String
joinColumns width sep leftLines rightLines =
  if diff < 0 then
    Array.zipWith go (leftLines <> Array.replicate (abs diff) "") rightLines
  else
    Array.zipWith go leftLines (rightLines <> Array.replicate diff "")
  where
  diff =
    Array.length leftLines - Array.length rightLines

  go left right = do
    let len = String.length left
    if len < width then
      left <> power " " (width - len) <> sep <> right
    else if len > width then
      String.take width left <> sep <> right
    else
      left <> sep <> right

printTableLines :: String -> Array (Array HelpFmt) -> Array String
printTableLines ind rows = do
  let
    rows' = map (renderDocLines "") <$> rows
    numCols = foldr (max <<< Array.length) 0 rows
    ixs = Array.range 0 (numCols - 1)
    colWidths =
      ixs # map \ix ->
        rows' # foldr
          ( flip Array.index ix
              >>> fromMaybe []
              >>> foldr (max <<< String.length) 0
              >>> max
          )
          0

  cols <- rows'
  case NonEmptyArray.fromArray (Array.zip colWidths cols) of
    Nothing -> []
    Just nea ->
      nea
        # NonEmptyArray.foldr1
            ( \(Tuple w1 col1) (Tuple w2 col2) ->
                Tuple (w1 + w2) (joinColumns w1 "    " col1 col2)
            )
        # snd
        # map (append ind)
