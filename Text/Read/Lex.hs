{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Read.Lex
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (uses Text.ParserCombinators.ReadP)
--
-- The cut-down Haskell lexer, used by Text.Read
--
-----------------------------------------------------------------------------

module Text.Read.Lex
--   -- lexing types
  ( Lexeme(..), Number
-- 
  , numberToInteger, numberToFixed, numberToRational, numberToRangedRational
-- 
--   -- lexer
  , lex, expect
  , hsLex
  , lexChar
-- 
--   , readIntP
--   , readOctP
--   , readDecP
--   , readHexP
-- 
--   , isSymbolChar
  )
 where
-- 
import Text.ParserCombinators.ReadP
-- 
import GHC.Base
import GHC.Char
import GHC.Num( Num(..), Integer(..) )
-- import GHC.Show( Show(..) )
import GHC.Unicode
  ( GeneralCategory(..), generalCategory, isSpace, isAlpha, isAlphaNum )
import GHC.Real( Rational, (%), fromIntegral, Integral,
                 toInteger, (^), quot, even )
import GHC.List
import GHC.Enum( minBound, maxBound )
import Data.Maybe

-- 
-- local copy to break import-cycle
-- | @'guard' b@ is @'return' ()@ if @b@ is 'True',
-- and 'mzero' if @b@ is 'False'.
guard           :: (MonadPlus m) => Bool -> m ()
guard True      =  return ()
guard False     =  mzero
-- 
-- -- -----------------------------------------------------------------------------
-- -- Lexing types
-- 
-- -- ^ Haskell lexemes.
data Lexeme
  = Char   Char         -- ^ Character literal
  | String String       -- ^ String literal, with escapes interpreted
  | Punc   String       -- ^ Punctuation or reserved symbol, e.g. @(@, @::@
  | Ident  String       -- ^ Haskell identifier, e.g. @foo@, @Baz@
  | Symbol String       -- ^ Haskell symbol, e.g. @>>@, @:%@
  | Number Number       -- ^ @since 4.6.0.0
  | EOF
--  deriving (Eq, Show)
instance Eq Lexeme where
    Char c1 == Char c2 = c1 == c2
    String s1 == String s2 = s1 == s2
    Punc s1 == Punc s2 = s1 == s2
    Ident s1 == Ident s2 = s1 == s2
    Symbol s1 == Symbol s2 = s1 == s2
    Number n1 == Number n2 = n1 == n2
    EOF == EOF = True
    _ == _ = False

-- 
-- -- | @since 4.7.0.0
data Number = MkNumber Int              -- Base
                       Digits           -- Integral part
            | MkDecimal Digits          -- Integral part
                        (Maybe Digits)  -- Fractional part
                        (Maybe Integer) -- Exponent
--  deriving (Eq, Show)
instance Eq Number where
    MkNumber i1 d1 == MkNumber i2 d2 = i1 == i2 && d1 == d2
    MkDecimal d1 md1 mi1 == MkDecimal d2 md2 mi2 = d1 == d2 && md1 == md2 && mi1 == mi2

-- | @since 4.5.1.0
numberToInteger :: Number -> Maybe Integer
numberToInteger (MkNumber base iPart) = Just (val (fromIntegral base) iPart)
numberToInteger (MkDecimal iPart Nothing Nothing) = Just (val (Z# 10#) iPart)
numberToInteger _ = Nothing

-- | @since 4.7.0.0
numberToFixed :: Integer -> Number -> Maybe (Integer, Integer)
numberToFixed _ (MkNumber base iPart) = Just (val (fromIntegral base) iPart, Z# 0#)
numberToFixed _ (MkDecimal iPart Nothing Nothing) = Just (val (Z# 10#) iPart, Z# 0#)
numberToFixed p (MkDecimal iPart (Just fPart) Nothing)
    = let i = val (Z# 10#) iPart
          f = val (Z# 10#) (integerTake p (fPart ++ repeat (I# 0#)))
          -- Sigh, we really want genericTake, but that's above us in
          -- the hierarchy, so we define our own version here (actually
          -- specialised to Integer)
          integerTake             :: Integer -> [a] -> [a]
          integerTake n _ | n <= (Z# 0#) = []
          integerTake _ []        =  []
          integerTake n (x:xs)    =  x : integerTake (n-Z# 1#) xs
      in Just (i, f)
numberToFixed _ _ = Nothing

-- This takes a floatRange, and if the Rational would be outside of
-- the floatRange then it may return Nothing. Not that it will not
-- /necessarily/ return Nothing, but it is good enough to fix the
-- space problems in #5688
-- Ways this is conservative:
-- * the floatRange is in base 2, but we pretend it is in base 10
-- * we pad the floateRange a bit, just in case it is very small
--   and we would otherwise hit an edge case
-- * We only worry about numbers that have an exponent. If they don't
--   have an exponent then the Rational won't be much larger than the
--   Number, so there is no problem
-- | @since 4.5.1.0
numberToRangedRational :: (Int, Int) -> Number
                       -> Maybe Rational -- Nothing = Inf
numberToRangedRational (neg, pos) n@(MkDecimal iPart mFPart (Just exp))
    -- if exp is out of integer bounds,
    -- then the number is definitely out of range
    | exp > fromIntegral (maxBound :: Int) ||
      exp < fromIntegral (minBound :: Int)
    = Nothing
    | otherwise
    = let mFirstDigit = case dropWhile (I# 0# ==) iPart of
                        iPart'@(_ : _) -> Just (length iPart')
                        [] -> case mFPart of
                              Nothing -> Nothing
                              Just fPart ->
                                  case span (I# 0# ==) fPart of
                                  (_, []) -> Nothing
                                  (zeroes, _) ->
                                      Just (negate (length zeroes))
      in case mFirstDigit of
         Nothing -> Just (fromInteger (Z# 0#))
         Just firstDigit ->
             let firstDigit' = firstDigit + fromInteger exp
             in if firstDigit' > (pos + I# 3#)
                then Nothing
                else if firstDigit' < (neg - I# 3#)
                then Just (fromInteger (Z# 0#))
                else Just (numberToRational n)
numberToRangedRational _ n = Just (numberToRational n)

-- | @since 4.6.0.0
numberToRational :: Number -> Rational
numberToRational (MkNumber base iPart) = val (fromIntegral base) iPart % (Z# 1#)
numberToRational (MkDecimal iPart mFPart mExp)
 = let i = val (Z# 10#) iPart
   in case (mFPart, mExp) of
      (Nothing, Nothing)     -> i % (Z# 1#)
      (Nothing, Just exp)
       | exp >= (Z# 0#)            -> (i * (Z# 10# ^ exp)) % (Z# 1#)
       | otherwise           -> i % (Z# 10# ^ (Z# 0# - exp))
      (Just fPart, Nothing)  -> fracExp (Z# 0#)   i fPart
      (Just fPart, Just exp) -> fracExp exp i fPart
      -- fracExp is a bit more efficient in calculating the Rational.
      -- Instead of calculating the fractional part alone, then
      -- adding the integral part and finally multiplying with
      -- 10 ^ exp if an exponent was given, do it all at once.

-----------------------------------------------------------------------------
-- Lexing

lex :: ReadP Lexeme
lex = skipSpaces >> lexToken

-- | @since 4.7.0.0
expect :: Lexeme -> ReadP ()
expect lexeme = GHC.Base.do { skipSpaces
                   ; thing <- lexToken
                   ; if thing == lexeme then return () else pfail }

hsLex :: ReadP String
-- ^ Haskell lexer: returns the lexed string, rather than the lexeme
hsLex = GHC.Base.do
           skipSpaces
           (s,_) <- gather lexToken
           return s

lexToken :: ReadP Lexeme
lexToken = lexEOF     +++
           lexLitChar +++
           lexString  +++
           lexPunc    +++
           lexSymbol  +++
           lexId      +++
           lexNumber


-- ----------------------------------------------------------------------
-- End of file
lexEOF :: ReadP Lexeme
lexEOF = GHC.Base.do
            s <- look
            guard (null s)
            return EOF

-- ---------------------------------------------------------------------------
-- Single character lexemes

lexPunc :: ReadP Lexeme
lexPunc =
  GHC.Base.do
     c <- satisfy isPuncChar
     return (Punc [c])

-- | The @special@ character class as defined in the Haskell Report.
isPuncChar :: Char -> Bool
isPuncChar c = c `elem` map char2char ",;()[]{}`"

----------------------------------------------------------------------
-- Symbols

lexSymbol :: ReadP Lexeme
lexSymbol =
  GHC.Base.do
     s <- munch1 isSymbolChar
     if s `elem` reserved_ops then
        return (Punc s)         -- Reserved-ops count as punctuation
      else
        return (Symbol s)
  where
    reserved_ops   = [map char2char "..", map char2char "::", map char2char "=", map char2char "\\", map char2char "|", map char2char "<-", map char2char "->", map char2char "@", map char2char "~", map char2char "=>"]

isSymbolChar :: Char -> Bool
isSymbolChar c = not (isPuncChar c) && case generalCategory c of
    MathSymbol              -> True
    CurrencySymbol          -> True
    ModifierSymbol          -> True
    OtherSymbol             -> True
    DashPunctuation         -> True
    OtherPunctuation        -> not (c `elem` map char2char "'\"")
    ConnectorPunctuation    -> c /= C# '_'#
    _                       -> False
-- ----------------------------------------------------------------------
-- identifiers

lexId :: ReadP Lexeme
lexId = GHC.Base.do
           c <- satisfy isIdsChar
           s <- munch isIdfChar
           return (Ident (c:s))
  where
          -- Identifiers can start with a '_'
    isIdsChar c = isAlpha c || c == C# '_'#
    isIdfChar c = isAlphaNum c || c `elem` map char2char "_'"

-- ---------------------------------------------------------------------------
-- Lexing character literals

lexLitChar :: ReadP Lexeme
lexLitChar = GHC.Base.do
     _ <- char (C# '\''#)
     (c,esc) <- lexCharE
     guard (esc || c /= C# '\''#)   -- Eliminate '' possibility
     _ <- char (C# '\''#)
     return (Char c)

lexChar :: ReadP Char
lexChar = GHC.Base.do { (c,_) <- lexCharE; consumeEmpties; return c }
    where
    -- Consumes the string "\&" repeatedly and greedily (will only produce one match)
    consumeEmpties :: ReadP ()
    consumeEmpties = GHC.Base.do
        rest <- look
        case rest of
            (C# '\\'#:C# '&'#:_) -> string (map char2char "\\&") >> consumeEmpties
            _ -> return ()


lexCharE :: ReadP (Char, Bool)  -- "escaped or not"?
lexCharE =
  GHC.Base.do
     c1 <- get
     if c1 == C# '\\'#
       then GHC.Base.do c2 <- lexEsc; return (c2, True)
       else GHC.Base.do return (c1, False)
 where
  lexEsc =
    lexEscChar
      +++ lexNumeric
        +++ lexCntrlChar
          +++ lexAscii

  lexEscChar =
    GHC.Base.do
       c <- get
       case unpackChar c of
         'a'#  -> return $ char2char '\a'
         'b'#  -> return $ char2char '\b'
         'f'#  -> return $ char2char '\f'
         'n'#  -> return $ char2char '\n'
         'r'#  -> return $ char2char '\r'
         't'#  -> return $ char2char '\t'
         'v'#  -> return $ char2char '\v'
         '\\'# -> return $ char2char '\\'
         '\"'# -> return $ char2char '\"'
         '\''# -> return $ char2char '\''
         _    -> pfail

  lexNumeric =
    GHC.Base.do
       base <- lexBaseChar <++ return (I# 10#)
       n    <- lexInteger base
       guard (n <= toInteger (ord maxBound))
       return (chr (fromInteger n))

  lexCntrlChar =
    GHC.Base.do
       _ <- char (C# '^'#)
       c <- get
       case unpackChar c of
         '@'#  -> return $ char2char '\^@'
         'A'#  -> return $ char2char '\^A'
         'B'#  -> return $ char2char '\^B'
         'C'#  -> return $ char2char '\^C'
         'D'#  -> return $ char2char '\^D'
         'E'#  -> return $ char2char '\^E'
         'F'#  -> return $ char2char '\^F'
         'G'#  -> return $ char2char '\^G'
         'H'#  -> return $ char2char '\^H'
         'I'#  -> return $ char2char '\^I'
         'J'#  -> return $ char2char '\^J'
         'K'#  -> return $ char2char '\^K'
         'L'#  -> return $ char2char '\^L'
         'M'#  -> return $ char2char '\^M'
         'N'#  -> return $ char2char '\^N'
         'O'#  -> return $ char2char '\^O'
         'P'#  -> return $ char2char '\^P'
         'Q'#  -> return $ char2char '\^Q'
         'R'#  -> return $ char2char '\^R'
         'S'#  -> return $ char2char '\^S'
         'T'#  -> return $ char2char '\^T'
         'U'#  -> return $ char2char '\^U'
         'V'#  -> return $ char2char '\^V'
         'W'#  -> return $ char2char '\^W'
         'X'#  -> return $ char2char '\^X'
         'Y'#  -> return $ char2char '\^Y'
         'Z'#  -> return $ char2char '\^Z'
         '['#  -> return $ char2char '\^['
         '\\'# -> return $ char2char '\^\'
         ']'#  -> return $ char2char '\^]'
         '^'#  -> return $ char2char '\^^'
         '_'#  -> return $ char2char '\^_'
         _    -> pfail

  lexAscii =
    GHC.Base.do
        choice
         [ (string (map char2char "SOH") >> return (char2char '\SOH')) <++
           (string (map char2char "SO")  >> return (char2char '\SO'))
                -- \SO and \SOH need maximal-munch treatment
                -- See the Haskell report Sect 2.6

         , string (map char2char "NUL") >> return (char2char '\NUL')
         , string (map char2char "STX") >> return (char2char '\STX')
         , string (map char2char "ETX") >> return (char2char '\ETX')
         , string (map char2char "EOT") >> return (char2char '\EOT')
         , string (map char2char "ENQ") >> return (char2char '\ENQ')
         , string (map char2char "ACK") >> return (char2char '\ACK')
         , string (map char2char "BEL") >> return (char2char '\BEL')
         , string (map char2char "BS")  >> return (char2char '\BS')
         , string (map char2char "HT")  >> return (char2char '\HT')
         , string (map char2char "LF")  >> return (char2char '\LF')
         , string (map char2char "VT")  >> return (char2char '\VT')
         , string (map char2char "FF")  >> return (char2char '\FF')
         , string (map char2char "CR")  >> return (char2char '\CR')
         , string (map char2char "SI")  >> return (char2char '\SI')
         , string (map char2char "DLE") >> return (char2char '\DLE')
         , string (map char2char "DC1") >> return (char2char '\DC1')
         , string (map char2char "DC2") >> return (char2char '\DC2')
         , string (map char2char "DC3") >> return (char2char '\DC3')
         , string (map char2char "DC4") >> return (char2char '\DC4')
         , string (map char2char "NAK") >> return (char2char '\NAK')
         , string (map char2char "SYN") >> return (char2char '\SYN')
         , string (map char2char "ETB") >> return (char2char '\ETB')
         , string (map char2char "CAN") >> return (char2char '\CAN')
         , string (map char2char "EM")  >> return (char2char '\EM')
         , string (map char2char "SUB") >> return (char2char '\SUB')
         , string (map char2char "ESC") >> return (char2char '\ESC')
         , string (map char2char "FS")  >> return (char2char '\FS')
         , string (map char2char "GS")  >> return (char2char '\GS')
         , string (map char2char "RS")  >> return (char2char '\RS')
         , string (map char2char "US")  >> return (char2char '\US')
         , string (map char2char "SP")  >> return (char2char '\SP')
         , string (map char2char "DEL") >> return (char2char '\DEL')
         ]


-- ---------------------------------------------------------------------------
-- string literal

lexString :: ReadP Lexeme
lexString =
  GHC.Base.do
     _ <- char (C# '"'#)
     body id
 where
  body f =
    GHC.Base.do
       (c,esc) <- lexStrItem
       if c /= C# '"'# || esc
         then body (f.(c:))
         else let s = f (map char2char "") in
              return (String s)

  lexStrItem = (lexEmpty >> lexStrItem)
               +++ lexCharE

  lexEmpty =
    GHC.Base.do
       _ <- char (C# '\\'#)
       c <- get
       case unpackChar c of
         '&'#          -> GHC.Base.do return ()
         _ | isSpace c -> GHC.Base.do skipSpaces; _ <- char (C# '\\'#); return ()
         _             -> GHC.Base.do pfail

-- ---------------------------------------------------------------------------
--  Lexing numbers

type Base   = Int
type Digits = [Int]

lexNumber :: ReadP Lexeme
lexNumber
  = lexHexOct  <++      -- First try for hex or octal 0x, 0o etc
                        -- If that fails, try for a decimal number
    lexDecNumber        -- Start with ordinary digits

lexHexOct :: ReadP Lexeme
lexHexOct
  = GHC.Base.do
        _ <- char (C# '0'#)
        base <- lexBaseChar
        digits <- lexDigits base
        return (Number (MkNumber base digits))

lexBaseChar :: ReadP Int
-- Lex a single character indicating the base; fail if not there
lexBaseChar = GHC.Base.do { c <- get;
                   case unpackChar c of
                        'o'# -> return (I# 8#)
                        'O'# -> return (I# 8#)
                        'x'# -> return (I# 16#)
                        'X'# -> return (I# 16#)
                        _   -> pfail }

lexDecNumber :: ReadP Lexeme
lexDecNumber =
  GHC.Base.do
     xs    <- lexDigits (I# 10#)
     mFrac <- lexFrac <++ return Nothing
     mExp  <- lexExp  <++ return Nothing
     return (Number (MkDecimal xs mFrac mExp))

lexFrac :: ReadP (Maybe Digits)
-- Read the fractional part; fail if it doesn't
-- start ".d" where d is a digit
lexFrac = GHC.Base.do
             _ <- char (char2char '.')
             fraction <- lexDigits (I# 10#)
             return (Just fraction)

lexExp :: ReadP (Maybe Integer)
lexExp =   GHC.Base.do
            _ <- char (char2char 'e') +++ char (char2char 'E')
            exp <- signedExp +++ lexInteger (I# 10#)
            return (Just exp)
 where
   signedExp
     =  GHC.Base.do
          c <- char (char2char '-') +++ char (char2char  '+')
          n <- lexInteger (I# 10#)
          return (if c == char2char '-' then Z# 0# - n else n)

lexDigits :: Int -> ReadP Digits
-- Lex a non-empty sequence of digits in specified base
lexDigits base =
  GHC.Base.do
     s  <- look
     xs <- scan s id
     guard (not (null xs))
     return xs
 where
  scan (c:cs) f = case valDig base c of
                    Just n  -> GHC.Base.do _ <- get; scan cs (f.(n:))
                    Nothing -> GHC.Base.do return (f [])
  scan []     f = GHC.Base.do return (f [])

lexInteger :: Base -> ReadP Integer
lexInteger base =
  GHC.Base.do
     xs <- lexDigits base
     return (val (fromIntegral base) xs)

val :: Num a => a -> Digits -> a
val = valSimple
{-# RULES
"val/Integer" val = valInteger
  #-}
{-# INLINE [1] val #-}

-- The following algorithm is only linear for types whose Num operations
-- are in constant time.
valSimple :: (Num a, Integral d) => a -> [d] -> a
valSimple base = go (fromInteger (Z# 0#))
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d
{-# INLINE valSimple #-}

-- A sub-quadratic algorithm for Integer. Pairs of adjacent radix b
-- digits are combined into a single radix b^2 digit. This process is
-- repeated until we are left with a single digit. This algorithm
-- performs well only on large inputs, so we use the simple algorithm
-- for smaller inputs.
valInteger :: Integer -> Digits -> Integer
valInteger b0 ds0 = go b0 (length ds0) $ map fromIntegral ds0
  where
    go _ _ []  = Z# 0#
    go _ _ [d] = d
    go b l ds
        | l > I# 40# = b' `seq` go b' l' (combine b ds')
        | otherwise = valSimple b ds
      where
        -- ensure that we have an even number of digits
        -- before we call combine:
        ds' = if even l then ds else Z# 0# : ds
        b' = b * b
        l' = (l + I# 1#) `quot` I# 2#
    combine b (d1 : d2 : ds) = d `seq` (d : combine b ds)
      where
        d = d1 * b + d2
    combine _ []  = []
    combine _ [_] = errorWithoutStackTrace "this should not happen"

-- Calculate a Rational from the exponent [of 10 to multiply with],
-- the integral part of the mantissa and the digits of the fractional
-- part. Leaving the calculation of the power of 10 until the end,
-- when we know the effective exponent, saves multiplications.
-- More importantly, this way we need at most one gcd instead of three.
--
-- frac was never used with anything but Integer and base 10, so
-- those are hardcoded now (trivial to change if necessary).
fracExp :: Integer -> Integer -> Digits -> Rational
fracExp exp mant []
  | exp < Z# 0#     = mant % (Z# 10# ^ (Z# 0# - exp))
  | otherwise   = fromInteger (mant * (Z# 10#) ^ exp)
fracExp exp mant (d:ds) = exp' `seq` mant' `seq` fracExp exp' mant' ds
  where
    exp'  = exp - Z# 1#
    mant' = mant * (Z# 10#) + fromIntegral d

valDig :: (Eq a, Num a) => a -> Char -> Maybe Int
valDig x c
  | x == fromInteger (Z# 8#), char2char '0' <= c && c <= char2char '7' = Just (ord c - ord (char2char '0'))
  | x == fromInteger (Z# 8#)            = Nothing

valDig x c | x == fromInteger (Z# 10#) = valDecDig c

valDig x c
  | x == fromInteger (Z# 16#), char2char '0' <= c && c <= char2char '9' = Just (ord c - ord (char2char '0'))
  | x == fromInteger (Z# 16#), char2char 'a' <= c && c <= char2char 'f' = Just (ord c - ord (char2char 'a') + (fromInteger (Z# 10#)))
  | x == fromInteger (Z# 16#), char2char 'A' <= c && c <= char2char 'F' = Just (ord c - ord (char2char 'A') + (fromInteger (Z# 10#)))
  | x == fromInteger (Z# 16#)            = Nothing

valDig _ _ = errorWithoutStackTrace "valDig: Bad base"

valDecDig :: Char -> Maybe Int
valDecDig c
  | char2char '0' <= c && c <= char2char '9' = Just (ord c - ord (char2char '0'))
  | otherwise            = Nothing

-- ----------------------------------------------------------------------
-- other numeric lexing functions

readIntP :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
readIntP base isDigit valDigit =
  GHC.Base.do
    s <- munch1 isDigit
    return (val base (map valDigit s))
{-# SPECIALISE readIntP
        :: Integer -> (Char -> Bool) -> (Char -> Int) -> ReadP Integer #-}

readIntP' :: (Eq a, Num a) => a -> ReadP a
readIntP' base = readIntP base isDigit valDigit
 where
  isDigit  c = maybe False (const True) (valDig base c)
  valDigit c = maybe (fromInteger (Z# 0#))     id           (valDig base c)
{-# SPECIALISE readIntP' :: Integer -> ReadP Integer #-}

readOctP, readDecP, readHexP :: (Eq a, Num a) => ReadP a
readOctP = readIntP' (fromInteger (Z# 8#))
readDecP = readIntP' (fromInteger (Z# 10#))
readHexP = readIntP' (fromInteger (Z# 16#))
{-# SPECIALISE readOctP :: ReadP Integer #-}
{-# SPECIALISE readDecP :: ReadP Integer #-}
{-# SPECIALISE readHexP :: ReadP Integer #-}
