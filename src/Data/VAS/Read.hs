module Data.VAS.Read (readSpec, Spec(..), specToVAS) where

import Data.VAS
import Data.Void
import System.FilePath.Posix
import Data.Function ((&))

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShowId, trace)

-- | The specification (as read from a file) always equips a VAS with some
-- initial configuration and some target configuration.
data Spec = Spec
    { vas     :: VAS
    , initial :: Configuration
    , target  :: Configuration
    }

specToVAS :: Spec -> Configured VAS
specToVAS (Spec v i t) = (v,i)

instance Show Spec where
    show (Spec vas initial target) = unlines
        [ "VAS: "
        , show vas
        , "Initial Configuration:"
        , show initial
        , "", "Target Configuration:"
        , show target ]

-- | Read a Vector Addition System from a given file path.
readSpec :: String -> IO Spec
readSpec path = do
    text <- readFile path
    case takeExtension path of
        ".spec" -> case parse specParser path text of
            Left  errs -> error $ errorBundlePretty errs
            Right spec -> return spec
        _       -> error "Only .spec files can be read at present."

-- * VAS Parser
-- This is written with the Megaparsec library.

-- | We can specialise our parser type over strings.
type Parser = Parsec Void String

-- | The primary parsing function which ties everything together.
specParser :: Parser Spec
specParser = do
    optional sc -- allow for a comment right at the start!
    vars   <- sectionVars
    rules  <- sectionRules
    init   <- sectionInit
    target <- sectionTarget
    optional sectionInvariants
    takeRest

    return Spec {
        vas     = VAS $ getFlow vars rules,
        initial = vectorise vars init,
        target  = vectorise vars target
    }

    where
        -- | Convert from the rules-based description to (pre,post) vectors.
        getFlow vars = fmap (convertRule vars)
        convertRule :: Vector String -> (Map String Int, Map String Int) -> (V, V)
        convertRule vars (g,d) = (vectorise vars g, vectorise vars d)
                         & uncurry Vector.zip
                         & fmap modify
                         & Vector.unzip

        -- | Change from a delta to a (pre,post) form with all values >= 0.
        modify :: (Int, Int) -> (Int, Int)
        modify (guard, delta)
            | delta > 0 = (guard                , guard + delta        )
            | otherwise = (max guard (abs delta), max 0 (guard + delta))

        -- | Convert from a sparse representation to a fully populated one.
        vectorise :: Vector String -> Map String Int -> V
        vectorise vars values = (\k -> Map.findWithDefault 0 k values) <$> vars


sectionVars :: Parser (Vector String)
sectionVars  = symbol "vars"  >> Vector.fromList <$> many placename


sectionRules :: Parser (Vector (Map String Int, Map String Int))
sectionRules = symbol "rules" >> Vector.fromList <$> many rule
    where
        rule = do
            guards  <- guard  `sepBy` (symbol ",")
            symbol "->"
            updates <- update `sepBy` (symbol ",")
            symbol ";"
            return (Map.fromList guards, Map.fromList updates)

        guard = pair ">="

        update = do
            p <- placename
            symbol "'"
            symbol "="
            placename
            plusMinus <- symbol "+" <|> symbol "-"
            i <- nat
            let op = case plusMinus of { "+" -> (+); "-" -> (-) }
            return (p, 0 `op` i)


sectionInit :: Parser (Map String Int)
sectionInit = symbol "init" >> Map.fromList <$> (try eq <|> gte) `sepBy` symbol ","

sectionTarget :: Parser (Map String Int)
sectionTarget = symbol "target" >> Map.fromList <$> gte `sepBy` symbol ","

sectionInvariants :: Parser ()
sectionInvariants = do
    symbol "invariants"
    return $ trace "Sorry, invariants are not supported. They will be ignored." ()

pair :: String -> Parser (String, Int)
pair sym = do
    p <- placename
    symbol sym
    i <- nat
    return (p,i)

eq  = pair "="
gte = pair ">="

-- * Lexer
-- This defines the consituent parts of the lexer for `.spec` files.

-- | Eats spaces and comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

-- | One element of the language.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | A symbol is built of lexemes and may be followed by whitespace.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Our numbers (as written in the spec) are always positive integers.
nat :: Parser Int
nat = lexeme L.decimal

-- | Placenames must not be part of the reserved list,
-- but can be most other things. This is more restrictive than necessary.
placename :: Parser String
placename = lexeme $ try $ ident >>= check
    where
        ident = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf ("_"::String))
        check i = if i `elem` ["vars","rules","init","target","invariants"]
                  then fail "This identifier is reserved"
                  else return i
