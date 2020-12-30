-- Copyright 2004, The University Court of the University of Glasgow.
-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment top"

{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Lexer (
   Token(..), lexer, mkPState, mkPStatePure, PState(..),
   P(..), ParseResult(..),
   getRealSrcLoc, getPState,
   failLocMsgP, failSpanMsgP, srcParseFail,
   popContext, pushModuleContext, setLastToken,
   activeContext,
   getLexState, popLexState, pushLexState,
   lexTokenStream, getLineNumber, getCol
  ) where

-- base
import Control.Monad
import Control.Monad.Fail as MonadFail
import Data.Char
import Data.List
import Data.Maybe
import Data.Word

-- containers
import qualified Data.Map as Map

-- compiler/utils
import Outputable
import StringBuffer
import FastString
import Util             ( readRational )

-- compiler/main
import ErrUtils

-- compiler/basicTypes
import SrcLoc
import BasicTypes     ( IntegralLit(..), FractionalLit(..),
                        SourceText(..) )

-- compiler/parser
import Ctype

}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$nl          = [\n\r\f]
$whitechar   = [$nl\v\ ]
$white_no_nl = $whitechar # \n 
$tab         = \t

$decdigit  = 0-9 -- for now, should really be $digit (ToDo)
$digit     = [$decdigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$symbol    = [$ascsymbol] # [$special \_\"\']

$large  = [A-Z]

$ascsmall  = [a-z]
$small     = [$ascsmall \_]

$graphic   = [$small $large $symbol $digit $special \"\']

$octit     = 0-7
$hexit     = [$decdigit A-F a-f]

$idchar    = [$small $large $digit \']

-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

@varid     = $small $idchar*          -- variable identifiers
@conid     = $large $idchar*          -- constructor identifiers

@varsym    = ($symbol # \:) $symbol*  -- variable (operator) symbol
@consym    = \: $symbol*              -- constructor (operator) symbol

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

@qual = (@conid \.)+
@qvarid = @qual @varid
@qconid = @qual @conid
@qvarsym = @qual @varsym
@qconsym = @qual @consym

@negative = \-

-- -----------------------------------------------------------------------------
-- Alex "Identifier"

haskell :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

--"

-- everywhere: skip whitespace
$white_no_nl+ ;
$tab          ;

"{-" { nested_comment lexToken }

<0, bol, layout> "--"\-*[^$symbol].*		;
<0, bol, layout> "--" $whitechar* ;

-- 'bol' state: beginning of a line.  Slurp up all the whitespace (including
-- blank lines) until we find a non-whitespace character, then do layout
-- processing.
<bol> {
  \n                                    ;
  ()                                    { do_bol }
}

-- after a layout keyword (let, where, do, of), we begin a new layout
-- context if the curly brace is missing.
-- Careful! This stuff is quite delicate.
<layout, layout_do> {
  \{ / { notFollowedBy '-' }            { hopefully_open_brace }
  \n                                    ;
}

-- do is treated in a subtly different way, see new_layout_context
<layout>    ()                          { new_layout_context True  generateSemic ITvocurly }
<layout_do> ()                          { new_layout_context False generateSemic ITvocurly }

-- after a new layout context which was found to be to the left of the
-- previous context, we have generated a '{' token, and we now need to
-- generate a matching '}' token.
<layout_left>  ()                       { do_layout_left }

<0> \n                     { begin bol }

<0> {
  \(                                    { special IToparen }
  \)                                    { special ITcparen }
  \[                                    { special ITobrack }
  \]                                    { special ITcbrack }
  \,                                    { special ITcomma }
  \;                                    { special ITsemi }
  \`                                    { special ITbackquote }
  \{                                    { open_brace }
  \}                                    { close_brace }
}

<0> {
  @qvarid                       { idtoken qvarid }
  @qconid                       { idtoken qconid }
  @varid                        { varid }
  @conid                        { idtoken conid }
}

<0> {
  @qvarsym                                         { idtoken qvarsym }
  @qconsym                                         { idtoken qconsym }
  @varsym                                          { varsym }
  @consym                                          { consym }
}

<0> {
  @decimal { tok_num positive 0 0 decimal }
  0[oO] @octal { tok_num positive 2 2 octal }
  0[xX] @hexadecimal		{ tok_num positive 2 2 hexadecimal }

  @negative @decimal { tok_num negative 1 1 decimal }
  @negative 0[oO] @octal { tok_num negative 3 3 octal }
  @negative 0[xX] @hexadecimal		{ tok_num negative 3 3 hexadecimal }

  @decimal \. @decimal @exponent? { tok_frac 0 tok_float }
  @decimal @exponent		{ tok_frac 0 tok_float }

  @negative @decimal \. @decimal @exponent? { tok_frac 0 tok_float }
  @negative @decimal @exponent		{ tok_frac 0 tok_float }
}


-- Strings and chars are lexed by hand-written code.  The reason is
-- that even if we recognise the string or char here in the regex
-- lexer, we would still have to parse the string afterward in order
-- to convert it to a String.
<0> {
  \'                            { lex_char_tok }
  \"                            { lex_string_tok }
}

-- '"\


-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{

-- -----------------------------------------------------------------------------
-- The token type

data Token
  = ITas                        -- Haskell keywords
  | ITcase
  | ITclass
  | ITdata
  | ITderiving
  | ITdo
  | ITelse
  | IThiding
  | ITforeign
  | ITif
  | ITimport
  | ITin
  | ITinfix
  | ITinfixl
  | ITinfixr
  | ITinstance
  | ITlet
  | ITmodule
  | ITnewtype
  | ITof
  | ITqualified
  | ITthen
  | ITtype
  | ITwhere

  | ITdotdot                    -- reserved symbols
  | ITcolon
  | ITdcolon            
  | ITequal
  | ITlam
  | ITlcase
  | ITvbar
  | ITlarrow            
  | ITrarrow            
  | ITat
  | ITtilde
  | ITdarrow            
  | ITbang
  | ITdot
  | ITplus    
  | ITminus   
  | ITmult    
  | ITdiv     
  | ITgreater 
  | ITless    
  | ITdiff    
  | ITeqcomp  
  | ITgt      
  | ITlt      
  | ITor  
  | ITand 
  | ITappend 
  | ITpow 

  | ITocurly                    -- special symbols
  | ITccurly
  | ITvocurly
  | ITvccurly
  | ITobrack
  | ITcbrack
  | IToparen
  | ITcparen
  | ITsemi
  | ITcomma
  | ITunderscore
  | ITbackquote
  | ITsimpleQuote               --  '

  | ITvarid   String        -- identifiers
  | ITconid   String
  | ITvarsym  String
  | ITconsym  String
  | ITqvarid  (String,String)
  | ITqconid  (String,String)
  | ITqvarsym (String,String)
  | ITqconsym (String,String)

  | ITchar     SourceText Char       -- Note [Literal source text] in BasicTypes
  | ITstring   SourceText String -- Note [Literal source text] in BasicTypes
  | ITinteger  IntegralLit           -- Note [Literal source text] in BasicTypes
  | ITrational FractionalLit

  -- Template Haskell extension tokens
  | ITtyQuote                           --  ''

  | ITeof                        -- ^ end of file token

  deriving Show

instance Outputable Token where
  ppr x = text (show x)

reservedWordsFM :: [(String, Token)]
reservedWordsFM =
        [( "_",              ITunderscore     ),
         ( "as",             ITas             ),
         ( "case",           ITcase           ),
         ( "class",          ITclass          ),
         ( "data",           ITdata           ),
         ( "deriving",       ITderiving       ),
         ( "do",             ITdo             ),
         ( "else",           ITelse           ),
         ( "hiding",         IThiding         ),
         ( "if",             ITif             ),
         ( "import",         ITimport         ),
         ( "in",             ITin             ),
         ( "infix",          ITinfix          ),
         ( "infixl",         ITinfixl         ),
         ( "infixr",         ITinfixr         ),
         ( "instance",       ITinstance       ),
         ( "let",            ITlet            ),
         ( "module",         ITmodule         ),
         ( "newtype",        ITnewtype        ),
         ( "of",             ITof             ),
         ( "qualified",      ITqualified      ),
         ( "then",           ITthen           ),
         ( "type",           ITtype           ),
         ( "where",          ITwhere          )
     ]

reservedSymsFM :: [(String, Token)]
reservedSymsFM = 
      [ ("..",  ITdotdot                  )
       ,(":",   ITcolon                   )
       ,("::",  ITdcolon                  )
       ,("=",   ITequal                   )
       ,("\\",  ITlam                     )
       ,("|",   ITvbar                    )
       ,("<-",  ITlarrow                  )
       ,("->",  ITrarrow                  )
       ,("@",   ITat                      )
       ,("~",   ITtilde                   )
       ,("=>",  ITdarrow                  )
       ,("!",   ITbang                    )
       ,("+",   ITplus                    )
       ,("-",   ITminus                   )
       ,("*",   ITmult                    )
       ,("/",   ITdiv                     )
       ,(">",   ITgreater                 )
       ,("<",   ITless                    )
       ,("/=",  ITdiff                    )
       ,("==",  ITeqcomp                  )
       ,(">=",  ITgt                      )
       ,("<=",  ITlt                      )
       ,("||",  ITor                      )
       ,("&&",  ITand                     )
       ,("++",  ITappend                  )
       ,("^",   ITpow                     )
       ]

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = RealSrcSpan -> StringBuffer -> Int -> P (RealLocated Token)

special :: Token -> Action
special tok span _buf _len = return (L span tok)

token, layout_token :: Token -> Action
token t span _buf _len = return (L span t)
layout_token t span _buf _len = pushLexState layout >> return (L span t)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len = return (L span $! (f buf len))

strtoken :: (String -> Token) -> Action
strtoken f span buf len =
  return (L span $! (f $! lexemeToString buf len))

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len = do _ <- popLexState
                         lexToken

hopefully_open_brace :: Action
hopefully_open_brace span buf len
 = do 
      ctx <- getContext
      (AI l _) <- getInput
      let offset = srcLocCol l
          isOK = case ctx of
                 Layout prev_off _ : _ -> prev_off < offset
                 _                     -> True
      if isOK then pop_and open_brace span buf len
              else failSpanMsgP (RealSrcSpan span) (text "Missing block")

pop_and :: Action -> Action
pop_and act span buf len = do _ <- popLexState
                              act span buf len

{-# INLINE nextCharIs #-}
nextCharIs :: StringBuffer -> (Char -> Bool) -> Bool
nextCharIs buf p = not (atEnd buf) && p (currentChar buf)

{-# INLINE nextCharIsNot #-}
nextCharIsNot :: StringBuffer -> (Char -> Bool) -> Bool
nextCharIsNot buf p = not (nextCharIs buf p)

notFollowedBy :: Char -> AlexAccPred a
notFollowedBy char _ _ _ (AI _ buf)
  = nextCharIsNot buf (== char)

{-
  nested comments require traversing by hand, they can't be parsed
  using regular expressions.
-}
nested_comment :: P (RealLocated Token) -> Action
nested_comment cont span buf len = do
  input <- getInput
  go (reverse $ lexemeToString buf len) (1::Int) input
  where
    go commentAcc 0 input = do
      setInput input
      cont
    go commentAcc n input = case alexGetChar input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar input of
        Nothing  -> errBrace input span
        Just ('\125',input) -> go ('\125':'-':commentAcc) (n-1) input -- '}'
        Just (_,_)          -> go ('-':commentAcc) n input
      Just ('\123',input) -> case alexGetChar input of  -- '{' char
        Nothing  -> errBrace input span
        Just ('-',input) -> go ('-':'\123':commentAcc) (n+1) input
        Just (_,_)       -> go ('\123':commentAcc) n input
      -- See Note [Nested comment line pragmas]
      Just ('\n',input) -> case alexGetChar input of
        Nothing  -> errBrace input span
        Just (_,_)   -> go ('\n':commentAcc) n input
      Just (c,input) -> go (c:commentAcc) n input

-------------------------------------------------------------------------------

errBrace :: AlexInput -> RealSrcSpan -> P a
errBrace (AI end _) span = failLocMsgP (realSrcSpanStart span) end "unterminated `{-'"

open_brace, close_brace :: Action
open_brace span _str _len = do
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (L span ITocurly)
close_brace span _str _len = do
  popContext
  return (L span ITccurly)

qvarid, qconid :: StringBuffer -> Int -> Token
qvarid buf len = ITqvarid $! splitQualName buf len False
qconid buf len = ITqconid $! splitQualName buf len False

splitQualName :: StringBuffer -> Int -> Bool -> (String,String)
-- takes a StringBuffer and a length, and returns the module name
-- and identifier parts of a qualified name.  Splits at the *last* dot,
-- because of hierarchical module names.
splitQualName orig_buf len parens = split orig_buf orig_buf
  where
    split buf dot_buf
        | orig_buf `byteDiff` buf >= len  = done dot_buf
        | c == '.'                        = found_dot buf'
        | otherwise                       = split buf' dot_buf
      where
       (c,buf') = nextChar buf

    -- careful, we might get names like M....
    -- so, if the character after the dot is not upper-case, this is
    -- the end of the qualifier part.
    found_dot buf -- buf points after the '.'
        | isUpper c    = split buf' buf
        | otherwise    = done buf
      where
       (c,buf') = nextChar buf

    done dot_buf =
        (lexemeToString orig_buf (qual_size - 1),
         if parens -- Prelude.(+)
            then lexemeToString (stepOn dot_buf) (len - qual_size - 2)
            else lexemeToString dot_buf (len - qual_size))
      where
        qual_size = orig_buf `byteDiff` dot_buf

varid :: Action
varid span buf len =
  case lookup fs reservedWordsFM of
    Just ITcase -> do
      lastTk <- getLastTk
      keyword <- return ITcase
      maybe_layout keyword
      return $ L span keyword
    Just keyword -> do
      maybe_layout keyword
      return $ L span keyword
    Nothing ->
      return $ L span $ ITvarid fs
  where
    !fs = lexemeToString buf len

conid :: StringBuffer -> Int -> Token
conid buf len = ITconid $! lexemeToString buf len

qvarsym, qconsym :: StringBuffer -> Int -> Token
qvarsym buf len = ITqvarsym $! splitQualName buf len False
qconsym buf len = ITqconsym $! splitQualName buf len False

varsym, consym :: Action
varsym = sym ITvarsym
consym = sym ITconsym

sym :: (String -> Token) -> Action
sym con span buf len =
  case lookup fs reservedSymsFM  of
    Just keyword ->
      return $ L span keyword
    Nothing ->
      return $ L span $! con fs
  where
    !fs = lexemeToString buf len

-- Variations on the integral numeric literal.
tok_integral :: (SourceText -> Integer -> Token)
             -> (Integer -> Integer)
             -> Int -> Int
             -> (Integer, (Char -> Int))
             -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len = do
  let src = lexemeToString buf len
  if ('_' `elem` src)
    then failMsgP "Use NumericUnderscores to allow underscores in integer literals"
    else return $ L span $ itint (SourceText src)
       $! transint $ parseUnsignedInteger
       (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral $ \case
    st@(SourceText ('-':_)) -> itint st (const True)
    st@(SourceText _)       -> itint st (const False)
    st@NoSourceText         -> itint st (< 0)
  where
    itint :: SourceText -> (Integer -> Bool) -> Integer -> Token
    itint !st is_negative !val = ITinteger ((IL st $! is_negative val) val)

positive, negative :: (Integer -> Integer)
positive = id
negative = negate
decimal, octal, hexadecimal :: (Integer, Char -> Int)
decimal = (10,octDecDigit)
octal = (8,octDecDigit)
hexadecimal = (16,hexDigit)

-- readRational can understand negative rationals, exponents, everything.
tok_frac :: Int -> (String -> Token) -> Action
tok_frac drop f span buf len = do
  let src = lexemeToString buf (len-drop)
  if ('_' `elem` src)
    then failMsgP "Use NumericUnderscores to allow underscores in floating literals"
    else return (L span $! (f $! src))

tok_float :: String -> Token
tok_float        str = ITrational   $! readFractionalLit str

readFractionalLit :: String -> FractionalLit
readFractionalLit str = ((FL $! (SourceText str)) $! is_neg) $! readRational str
                        where is_neg = case str of ('-':_) -> True
                                                   _       -> False

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol span _str _len = do
        (pos, gen_semic) <- getOffside
        case pos of
            LT -> do
                --trace "layout: inserting '}'" $ do
                popContext
                -- do NOT pop the lex state, we might have a ';' to insert
                return (L span ITvccurly)
            EQ | gen_semic -> do
                --trace "layout: inserting ';'" $ do
                _ <- popLexState
                return (L span ITsemi)
            _ -> do
                _ <- popLexState
                lexToken

-- certain keywords put us in the "layout" state, where we might
-- add an opening curly brace.
maybe_layout :: Token -> P ()
maybe_layout t = do -- If the alternative layout rule is enabled then
                    -- we never create an implicit layout context here.
                    -- Layout is handled XXX instead.
                    -- The code for closing implicit contexts, or
                    -- inserting implicit semi-colons, is therefore
                    -- irrelevant as it only applies in an implicit
                    -- context.
                    f t
    where f ITdo    = pushLexState layout_do
          f ITlcase = pushLexState layout
          f ITof    = pushLexState layout
          f ITlet   = pushLexState layout
          f ITwhere = pushLexState layout
          f _       = return ()

-- Pushing a new implicit layout context.  If the indentation of the
-- next token is not greater than the previous layout context, then
-- Haskell 98 says that the new layout context should be empty; that is
-- the lexer must generate {}.
--
-- We are slightly more lenient than this: when the new context is started
-- by a 'do', then we allow the new context to be at the same indentation as
-- the previous context.  This is what the 'strict' argument is for.
new_layout_context :: Bool -> Bool -> Token -> Action
new_layout_context strict gen_semic tok span _buf len = do
    _ <- popLexState
    (AI l _) <- getInput
    let offset = srcLocCol l - len
    ctx <- getContext
    let strict' = strict || not False
    case ctx of
        Layout prev_off _ : _  |
           (strict'     && prev_off >= offset  ||
            not strict' && prev_off > offset) -> do
                -- token is indented to the left of the previous context.
                -- we must generate a {} sequence now.
                pushLexState layout_left
                return (L span tok)
        _ -> do setContext (Layout offset gen_semic : ctx)
                return (L span tok)

do_layout_left :: Action
do_layout_left span _buf _len = do
    _ <- popLexState
    pushLexState bol  -- we must be at the start of a line
    return (L span ITvccurly)

-- -----------------------------------------------------------------------------
-- Strings & Chars

lex_string_tok :: Action
lex_string_tok span buf _len = do
  tok <- lex_string ""
  (AI end bufEnd) <- getInput
  let
    tok' = case tok of
            ITstring _ s -> ITstring (SourceText src) s
            _ -> panic "lex_string_tok"
    src = lexemeToString buf (cur bufEnd - cur buf)
  return (L (mkRealSrcSpan (realSrcSpanStart span) end) tok')

lex_string :: String -> P Token
lex_string s = do
  i <- getInput
  case alexGetChar i of
    Nothing -> lit_error i
    Just ('"',i)  -> do
        setInput i
        return (ITstring (SourceText (reverse s))
                          ((reverse s)))

    Just ('\\',i)
        | Just ('&',i) <- next -> do
                setInput i; lex_string s
        | Just (c,i) <- next, c <= '\x7f' && is_space c -> do
                           -- is_space only works for <= '\x7f' (#3751, #5425)
                setInput i; lex_stringgap s
        where next = alexGetChar i

    Just (c, i1) -> do
        case c of
          '\\' -> do setInput i1; c' <- lex_escape; lex_string (c':s)
          c | isAny c -> do setInput i1; lex_string (c:s)
          _other -> lit_error i

lex_stringgap :: String -> P Token
lex_stringgap s = do
  i <- getInput
  c <- getCharOrFail i
  case c of
    '\\' -> lex_string s
    c | c <= '\x7f' && is_space c -> lex_stringgap s
                           -- is_space only works for <= '\x7f' (#3751, #5425)
    _other -> lit_error i


lex_char_tok :: Action
-- Here we are basically parsing character literals, such as 'x' or '\n'
-- but we additionally spot 'x and ''T, returning ITsimpleQuote and
-- ITtyQuote respectively, but WITHOUT CONSUMING the x or T part
-- (the parser does that).
-- So we have to do two characters of lookahead: when we see 'x we need to
-- see if there's a trailing quote
lex_char_tok span buf _len = do        -- We've seen '
   i1 <- getInput       -- Look ahead to first character
   let loc = realSrcSpanStart span
   case alexGetChar i1 of
        Nothing -> lit_error  i1

        Just ('\'', i2@(AI end2 _)) -> do       -- We've seen ''
                   setInput i2
                   return (L (mkRealSrcSpan loc end2)  ITtyQuote)

        Just ('\\', i2@(AI _end2 _)) -> do      -- We've seen 'backslash
                  setInput i2
                  lit_ch <- lex_escape
                  i3 <- getInput
                  mc <- getCharOrFail i3 -- Trailing quote
                  if mc == '\'' then finish_char_tok buf loc lit_ch
                                else lit_error i3

        Just (c, i2@(AI _end2 _))
                | not (isAny c) -> lit_error i1
                | otherwise ->

                -- We've seen 'x, where x is a valid character
                --  (i.e. not newline etc) but not a quote or backslash
           case alexGetChar i2 of      -- Look ahead one more character
                Just ('\'', i3) -> do   -- We've seen 'x'
                        setInput i3
                        finish_char_tok buf loc c
                _other -> do            -- We've seen 'x not followed by quote
                                        -- (including the possibility of EOF)
                                        -- Just parse the quote only
                        let (AI end _) = i1
                        return (L (mkRealSrcSpan loc end) ITsimpleQuote)

finish_char_tok :: StringBuffer -> RealSrcLoc -> Char -> P (RealLocated Token)
finish_char_tok buf loc ch  -- We've already seen the closing quote
                        -- Just need to check for trailing #
  = do  
      i@(AI end bufEnd) <- getInput
      let src = lexemeToString buf (cur bufEnd - cur buf)
      return (L (mkRealSrcSpan loc end) (ITchar (SourceText src) ch))

isAny :: Char -> Bool
isAny c | c > '\x7f' = isPrint c
        | otherwise  = is_any c

lex_escape :: P Char
lex_escape = do
  i0 <- getInput
  c <- getCharOrFail i0
  case c of
        'a'   -> return '\a'
        'b'   -> return '\b'
        'f'   -> return '\f'
        'n'   -> return '\n'
        'r'   -> return '\r'
        't'   -> return '\t'
        'v'   -> return '\v'
        '\\'  -> return '\\'
        '"'   -> return '\"'
        '\''  -> return '\''
        '^'   -> do i1 <- getInput
                    c <- getCharOrFail i1
                    if c >= '@' && c <= '_'
                        then return (chr (ord c - ord '@'))
                        else lit_error i1

        'x'   -> readNum is_hexdigit 16 hexDigit
        'o'   -> readNum is_octdigit  8 octDecDigit
        x | is_decdigit x -> readNum2 is_decdigit 10 octDecDigit (octDecDigit x)

        c1 ->  do
           i <- getInput
           case alexGetChar i of
            Nothing -> lit_error i0
            Just (c2,i2) ->
              case alexGetChar i2 of
                Nothing -> do lit_error i0
                Just (c3,i3) ->
                   let str = [c1,c2,c3] in
                   case [ (c,rest) | (p,c) <- silly_escape_chars,
                                     Just rest <- [stripPrefix p str] ] of
                          (escape_char,[]):_ -> do
                                setInput i3
                                return escape_char
                          (escape_char,_:_):_ -> do
                                setInput i2
                                return escape_char
                          [] -> lit_error i0

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Char
readNum is_digit base conv = do
  i <- getInput
  c <- getCharOrFail i
  if is_digit c
        then readNum2 is_digit base conv (conv c)
        else lit_error i

readNum2 :: (Char -> Bool) -> Int -> (Char -> Int) -> Int -> P Char
readNum2 is_digit base conv i = do
  input <- getInput
  read i input
  where read i input = do
          case alexGetChar input of
            Just (c,input') | is_digit c -> do
               let i' = i*base + conv c
               if i' > 0x10ffff
                  then setInput input >> lexError "numeric escape sequence out of range"
                  else read i' input'
            _other -> do
              setInput input; return (chr i)


silly_escape_chars :: [(String, Char)]
silly_escape_chars = [
        ("NUL", '\NUL'),
        ("SOH", '\SOH'),
        ("STX", '\STX'),
        ("ETX", '\ETX'),
        ("EOT", '\EOT'),
        ("ENQ", '\ENQ'),
        ("ACK", '\ACK'),
        ("BEL", '\BEL'),
        ("BS", '\BS'),
        ("HT", '\HT'),
        ("LF", '\LF'),
        ("VT", '\VT'),
        ("FF", '\FF'),
        ("CR", '\CR'),
        ("SO", '\SO'),
        ("SI", '\SI'),
        ("DLE", '\DLE'),
        ("DC1", '\DC1'),
        ("DC2", '\DC2'),
        ("DC3", '\DC3'),
        ("DC4", '\DC4'),
        ("NAK", '\NAK'),
        ("SYN", '\SYN'),
        ("ETB", '\ETB'),
        ("CAN", '\CAN'),
        ("EM", '\EM'),
        ("SUB", '\SUB'),
        ("ESC", '\ESC'),
        ("FS", '\FS'),
        ("GS", '\GS'),
        ("RS", '\RS'),
        ("US", '\US'),
        ("SP", '\SP'),
        ("DEL", '\DEL')
        ]

-- before calling lit_error, ensure that the current input is pointing to
-- the position of the error in the buffer.  This is so that we can report
-- a correct location to the user, but also so we can detect UTF-8 decoding
-- errors if they occur.
lit_error :: AlexInput -> P a
lit_error i = do setInput i; lexError "lexical error in string/character literal"

getCharOrFail :: AlexInput -> P Char
getCharOrFail i =  do
  case alexGetChar i of
        Nothing -> lexError "unexpected end-of-file in string/character literal"
        Just (c,i)  -> do setInput i; return c

-- -----------------------------------------------------------------------------
-- The Parse Monad

-- | Do we want to generate ';' layout tokens? In some cases we just want to
-- generate '}', e.g. in MultiWayIf we don't need ';'s because '|' separates
-- alternatives (unlike a `case` expression where we need ';' to as a separator
-- between alternatives).
type GenSemic = Bool

generateSemic, dontGenerateSemic :: GenSemic
generateSemic     = True
dontGenerateSemic = False

data LayoutContext
  = NoLayout
  | Layout !Int !GenSemic
  deriving Show

data ParseResult a
  = POk PState a
  | PFailed
        SrcSpan                -- The start and end of the text span related
                               -- to the error.  Might be used in environments
                               -- which can show this span, e.g. by
                               -- highlighting it.
        MsgDoc                 -- The error message

data PState = PState {
        buffer     :: StringBuffer,
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan, -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,  -- current loc (end of prev token + 1)
        context    :: [LayoutContext],
        lex_state  :: [Int],
        srcfiles   :: [String],
        -- Used in the alternative layout rule:
        -- These tokens are the next ones to be sent out. They are
        -- just blindly emitted, without the rule looking at them again:
        alr_pending_implicit_tokens :: [RealLocated Token],
        -- This is the next token to be considered or, if it is Nothing,
        -- we need to get the next token from the input stream:
        alr_next_token :: Maybe (RealLocated Token),
        -- This is what we consider to be the location of the last token
        -- emitted:
        alr_last_loc :: RealSrcSpan,
        -- The stack of layout contexts:
        alr_context :: [ALRContext],
        -- Are we expecting a '{'? If it's Just, then the ALRLayout tells
        -- us what sort of layout the '{' will open:
        alr_expecting_ocurly :: Maybe ALRLayout,
        -- Have we just had the '}' for a let block? If so, than an 'in'
        -- token doesn't need to close anything:
        alr_justClosedExplicitLetBlock :: Bool
     }
        -- last_loc and last_len are used when generating error messages,
        -- and in pushCurrentContext only.  Sigh, if only Happy passed the
        -- current token to happyError, we could at least get rid of last_len.
        -- Getting rid of last_loc would require finding another way to
        -- implement pushCurrentContext (which is only called from one place).

data ALRContext = ALRNoLayout Bool{- does it contain commas? -}
                              Bool{- is it a 'let' block? -}
                | ALRLayout ALRLayout Int
data ALRLayout = ALRLayoutLet
               | ALRLayoutWhere
               | ALRLayoutOf
               | ALRLayoutDo

newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = returnP
  (<*>) = ap

instance Monad P where
  (>>=) = thenP
#if !MIN_VERSION_base(4,13,0)
  fail = MonadFail.fail
#endif

instance MonadFail.MonadFail P where
  fail = failP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed span err -> PFailed span err

failP :: String -> P a
failP msg =
  P $ \s ->
    PFailed (RealSrcSpan (last_loc s)) (text msg)

failMsgP :: String -> P a
failMsgP msg =
  P $ \s ->
    PFailed (RealSrcSpan (last_loc s)) (text msg)

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> String -> P a
failLocMsgP loc1 loc2 str =
  P $ \s ->
    PFailed (RealSrcSpan (mkRealSrcSpan loc1 loc2)) (text str)

failSpanMsgP :: SrcSpan -> SDoc -> P a
failSpanMsgP span msg =
  P $ \s ->
    PFailed span msg

getPState :: P PState
getPState = P $ \s -> POk s s

getRealSrcLoc :: P RealSrcLoc
getRealSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

setLastToken :: RealSrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s {
  last_loc=loc,
  last_len=len
  } ()

setLastTk :: Token -> P ()
setLastTk tk = P $ \s -> POk s { last_tk = Just tk } ()

getLastTk :: P (Maybe Token)
getLastTk = P $ \s@(PState { last_tk = last_tk }) -> POk s last_tk

data AlexInput = AI RealSrcLoc StringBuffer

{-
Note [Unicode in Alex]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Although newer versions of Alex support unicode, this grammar is processed with
the old style '--latin1' behaviour. This means that when implementing the
functions

    alexGetByte       :: AlexInput -> Maybe (Word8,AlexInput)
    alexInputPrevChar :: AlexInput -> Char

which Alex uses to take apart our 'AlexInput', we must

  * return a latin1 character in the 'Word8' that 'alexGetByte' expects
  * return a latin1 character in 'alexInputPrevChar'.

We handle this in 'adjustChar' by squishing entire classes of unicode
characters into single bytes.
-}

{-# INLINE adjustChar #-}
adjustChar :: Char -> Word8
adjustChar c = fromIntegral $ ord adj_c
  where non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        uniidchar       = '\x07'

        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
          | otherwise =
                -- NB: The logic behind these definitions is also reflected
                -- in basicTypes/Lexeme.hs
                -- Any changes here should likely be reflected there.

                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic

-- Getting the previous 'Char' isn't enough here - we need to convert it into
-- the same format that 'alexGetByte' would have produced.
--
-- See Note [Unicode in Alex] and #13986.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = prevChar buf '\n'

-- See Note [Unicode in Alex]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = adjustChar c

-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (c, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s{ loc=l, buffer=b } ()

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

popNextToken :: P (Maybe (RealLocated Token))
popNextToken
    = P $ \s@PState{ alr_next_token = m } ->
              POk (s {alr_next_token = Nothing}) m

activeContext :: P Bool
activeContext = do
  ctxt <- getALRContext
  expc <- getAlrExpectingOCurly
  impt <- implicitTokenPending
  case (ctxt,expc) of
    ([],Nothing) -> return impt
    _other       -> return True

setAlrLastLoc :: RealSrcSpan -> P ()
setAlrLastLoc l = P $ \s -> POk (s {alr_last_loc = l}) ()

getAlrLastLoc :: P RealSrcSpan
getAlrLastLoc = P $ \s@(PState {alr_last_loc = l}) -> POk s l

getALRContext :: P [ALRContext]
getALRContext = P $ \s@(PState {alr_context = cs}) -> POk s cs

setALRContext :: [ALRContext] -> P ()
setALRContext cs = P $ \s -> POk (s {alr_context = cs}) ()

getJustClosedExplicitLetBlock :: P Bool
getJustClosedExplicitLetBlock
 = P $ \s@(PState {alr_justClosedExplicitLetBlock = b}) -> POk s b

setJustClosedExplicitLetBlock :: Bool -> P ()
setJustClosedExplicitLetBlock b
 = P $ \s -> POk (s {alr_justClosedExplicitLetBlock = b}) ()

setNextToken :: RealLocated Token -> P ()
setNextToken t = P $ \s -> POk (s {alr_next_token = Just t}) ()

implicitTokenPending :: P Bool
implicitTokenPending
    = P $ \s@PState{ alr_pending_implicit_tokens = ts } ->
              case ts of
              [] -> POk s False
              _  -> POk s True

popPendingImplicitToken :: P (Maybe (RealLocated Token))
popPendingImplicitToken
    = P $ \s@PState{ alr_pending_implicit_tokens = ts } ->
              case ts of
              [] -> POk s Nothing
              (t : ts') -> POk (s {alr_pending_implicit_tokens = ts'}) (Just t)

setPendingImplicitTokens :: [RealLocated Token] -> P ()
setPendingImplicitTokens ts = P $ \s -> POk (s {alr_pending_implicit_tokens = ts}) ()

getAlrExpectingOCurly :: P (Maybe ALRLayout)
getAlrExpectingOCurly = P $ \s@(PState {alr_expecting_ocurly = b}) -> POk s b

setAlrExpectingOCurly :: Maybe ALRLayout -> P ()
setAlrExpectingOCurly b = P $ \s -> POk (s {alr_expecting_ocurly = b}) ()

-- | Creates a parse state 
mkPState :: StringBuffer -> RealSrcLoc -> PState
mkPState buf loc = mkPStatePure buf loc

-- | Creates a parse state 
mkPStatePure :: StringBuffer -> RealSrcLoc -> PState
mkPStatePure buf loc =
  PState {
      buffer        = buf,
      last_tk       = Nothing,
      last_loc      = mkRealSrcSpan loc loc,
      last_len      = 0,
      loc           = loc,
      context       = [],
      lex_state     = [bol, 0],
      srcfiles      = [],
      alr_pending_implicit_tokens = [],
      alr_last_loc = mkRealSrcSpan loc loc,
      alr_next_token = Nothing,
      alr_context = [],
      alr_expecting_ocurly = Nothing,
      alr_justClosedExplicitLetBlock = False
    }

getContext :: P [LayoutContext]
getContext = P $ \s@PState{context=ctx} -> POk s ctx

setContext :: [LayoutContext] -> P ()
setContext ctx = P $ \s -> POk s{context=ctx} ()

popContext :: P ()
popContext = P $ \ s@(PState{ buffer = buf, {- options = o, -} context = ctx,
                              last_len = len, last_loc = last_loc }) ->
  case ctx of
        (_:tl) ->
          POk s{ context = tl } ()
        []     ->
          PFailed (RealSrcSpan last_loc) (srcParseErr buf len)

-- Push a new layout context at the indentation of the last token read.
pushCurrentContext :: GenSemic -> P ()
pushCurrentContext gen_semic = P $ \ s@PState{ last_loc=loc, context=ctx } ->
    POk s{context = Layout (srcSpanStartCol loc) gen_semic : ctx} ()

-- This is only used at the outer level of a module when the 'module' keyword is
-- missing.
pushModuleContext :: P ()
pushModuleContext = pushCurrentContext generateSemic

getOffside :: P (Ordering, Bool)
getOffside = P $ \s@PState{last_loc=loc, context=stk} ->
                let offs = srcSpanStartCol loc in
                let ord = case stk of
                            Layout n gen_semic : _ ->
                              --trace ("layout: " ++ show n ++ ", offs: " ++ show offs) $
                              (compare offs n, gen_semic)
                            _ ->
                              (GT, dontGenerateSemic)
                in POk s ord

-- ---------------------------------------------------------------------------
-- Construct a parse error

srcParseErr
  :: StringBuffer       -- current buffer (placed just after the last token)
  -> Int                -- length of the previous token
  -> MsgDoc
srcParseErr buf len
  = if null token
         then text "parse error (possibly incorrect indentation or mismatched brackets)"
         else text "parse error on input" <+> quotes (text token)
              $$ ppWhen (not False && token == "$") -- #7396
                        (text "Perhaps you intended to use TemplateHaskell")
              $$ ppWhen (token == "<-")
                        (if mdoInLast100
                           then text "Perhaps you intended to use RecursiveDo"
                           else text "Perhaps this statement should be within a 'do' block?")
              $$ ppWhen (token == "=")
                        (text "Perhaps you need a 'let' in a 'do' block?"
                         $$ text "e.g. 'let x = 5' instead of 'x = 5'")
              $$ ppWhen (not False && pattern == "pattern ")
                        (text "Perhaps you intended to use PatternSynonyms")
  where token = lexemeToString (offsetBytes (-len) buf) len
        pattern = decodePrevNChars 8 buf
        last100 = decodePrevNChars 100 buf
        mdoInLast100 = "mdo" `isInfixOf` last100

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \s@PState{ buffer = buf, last_len = len,
                            last_loc = last_loc } ->
    PFailed (RealSrcSpan last_loc) (srcParseErr buf len)

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.
lexError :: String -> P a
lexError str = do
  loc <- getRealSrcLoc
  (AI end buf) <- getInput
  reportLexError loc end buf str

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: Bool -> (Located Token -> P a) -> P a
lexer queueComments cont = do
  (L span tok) <- lexToken
  -- trace ("token: " ++ show tok) $ do
  cont (L (RealSrcSpan span) tok)

lexTokenAlr :: P (RealLocated Token)
lexTokenAlr = do mPending <- popPendingImplicitToken
                 t <- case mPending of
                      Nothing ->
                          do mNext <- popNextToken
                             t <- case mNext of
                                  Nothing -> lexToken
                                  Just next -> return next
                             alternativeLayoutRuleToken t
                      Just t ->
                          return t
                 setAlrLastLoc (getRealSrcSpan t)
                 case unRealSrcSpan t of
                     ITwhere -> setAlrExpectingOCurly (Just ALRLayoutWhere)
                     ITlet   -> setAlrExpectingOCurly (Just ALRLayoutLet)
                     ITlcase -> setAlrExpectingOCurly (Just ALRLayoutOf)
                     ITof    -> setAlrExpectingOCurly (Just ALRLayoutOf)
                     ITdo    -> setAlrExpectingOCurly (Just ALRLayoutDo)
                     _       -> return ()
                 return t

alternativeLayoutRuleToken :: RealLocated Token -> P (RealLocated Token)
alternativeLayoutRuleToken t
    = do context <- getALRContext
         lastLoc <- getAlrLastLoc
         mExpectingOCurly <- getAlrExpectingOCurly
         justClosedExplicitLetBlock <- getJustClosedExplicitLetBlock
         setJustClosedExplicitLetBlock False
         let thisLoc = getRealSrcSpan t
             thisCol = srcSpanStartCol thisLoc
             newLine = srcSpanStartLine thisLoc > srcSpanEndLine lastLoc
         case (unRealSrcSpan t, context, mExpectingOCurly) of
             -- This case handles a GHC extension to the original H98
             -- layout rule...
             (ITocurly, _, Just alrLayout) ->
                 do setAlrExpectingOCurly Nothing
                    let isLet = case alrLayout of
                                ALRLayoutLet -> True
                                _ -> False
                    setALRContext (ALRNoLayout (containsCommas ITocurly) isLet : context)
                    return t
             -- ...and makes this case unnecessary
             {-
             -- I think our implicit open-curly handling is slightly
             -- different to John's, in how it interacts with newlines
             -- and "in"
             (ITocurly, _, Just _) ->
                 do setAlrExpectingOCurly Nothing
                    setNextToken t
                    lexTokenAlr
             -}
             (_, ALRLayout _ col : _ls, Just expectingOCurly)
              | (thisCol > col) ||
                (thisCol == col &&
                 isNonDecreasingIndentation expectingOCurly) ->
                 do setAlrExpectingOCurly Nothing
                    setALRContext (ALRLayout expectingOCurly thisCol : context)
                    setNextToken t
                    return (L thisLoc ITocurly)
              | otherwise ->
                 do setAlrExpectingOCurly Nothing
                    setPendingImplicitTokens [L lastLoc ITccurly]
                    setNextToken t
                    return (L lastLoc ITocurly)
             (_, _, Just expectingOCurly) ->
                 do setAlrExpectingOCurly Nothing
                    setALRContext (ALRLayout expectingOCurly thisCol : context)
                    setNextToken t
                    return (L thisLoc ITocurly)
             -- We do the [] cases earlier than in the spec, as we
             -- have an actual EOF token
             (ITeof, ALRLayout _ _ : ls, _) ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITccurly)
             (ITeof, _, _) ->
                 return t
             -- the other ITeof case omitted; general case below covers it
             (ITin, _, _)
              | justClosedExplicitLetBlock ->
                 return t
             (ITin, ALRLayout ALRLayoutLet _ : ls, _)
              | newLine ->
                 do setPendingImplicitTokens [t]
                    setALRContext ls
                    return (L thisLoc ITccurly)
             (_, ALRLayout _ col : ls, _)
              | newLine && thisCol == col ->
                 do setNextToken t
                    return (L thisLoc ITsemi)
              | newLine && thisCol < col ->
                 do setALRContext ls
                    setNextToken t
                    -- Note that we use lastLoc, as we may need to close
                    -- more layouts, or give a semicolon
                    return (L lastLoc ITccurly)
             -- We need to handle close before open, as 'then' is both
             -- an open and a close
             (u, _, _)
              | isALRclose u ->
                 case context of
                 ALRLayout _ _ : ls ->
                     do setALRContext ls
                        setNextToken t
                        return (L thisLoc ITccurly)
                 ALRNoLayout _ isLet : ls ->
                     do let ls' = if isALRopen u
                                     then ALRNoLayout (containsCommas u) False : ls
                                     else ls
                        setALRContext ls'
                        when isLet $ setJustClosedExplicitLetBlock True
                        return t
                 [] ->
                     do let ls = if isALRopen u
                                    then [ALRNoLayout (containsCommas u) False]
                                    else []
                        setALRContext ls
                        -- XXX This is an error in John's code, but
                        -- it looks reachable to me at first glance
                        return t
             (u, _, _)
              | isALRopen u ->
                 do setALRContext (ALRNoLayout (containsCommas u) False : context)
                    return t
             (ITin, ALRLayout ALRLayoutLet _ : ls, _) ->
                 do setALRContext ls
                    setPendingImplicitTokens [t]
                    return (L thisLoc ITccurly)
             (ITin, ALRLayout _ _ : ls, _) ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITccurly)
             -- the other ITin case omitted; general case below covers it
             (ITcomma, ALRLayout _ _ : ls, _)
              | topNoLayoutContainsCommas ls ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITccurly)
             (ITwhere, ALRLayout ALRLayoutDo _ : ls, _) ->
                 do setALRContext ls
                    setPendingImplicitTokens [t]
                    return (L thisLoc ITccurly)
             -- the other ITwhere case omitted; general case below covers it
             (_, _, _) -> return t

isALRopen :: Token -> Bool
isALRopen ITcase          = True
isALRopen ITif            = True
isALRopen ITthen          = True
isALRopen IToparen        = True
isALRopen ITobrack        = True
isALRopen ITocurly        = True
isALRopen _               = False

isALRclose :: Token -> Bool
isALRclose ITof     = True
isALRclose ITthen   = True
isALRclose ITelse   = True
isALRclose ITcparen = True
isALRclose ITcbrack = True
isALRclose ITccurly = True
isALRclose _        = False

isNonDecreasingIndentation :: ALRLayout -> Bool
isNonDecreasingIndentation ALRLayoutDo = True
isNonDecreasingIndentation _           = False

containsCommas :: Token -> Bool
containsCommas IToparen = True
containsCommas ITobrack = True
-- John doesn't have {} as containing commas, but records contain them,
-- which caused a problem parsing Cabal's Distribution.Simple.InstallDirs
-- (defaultInstallDirs).
containsCommas ITocurly = True
containsCommas _        = False

topNoLayoutContainsCommas :: [ALRContext] -> Bool
topNoLayoutContainsCommas [] = False
topNoLayoutContainsCommas (ALRLayout _ _ : ls) = topNoLayoutContainsCommas ls
topNoLayoutContainsCommas (ALRNoLayout b _ : _) = b

lexToken :: P (RealLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  case alexScanUser (fromIntegral 0) inp sc of -- Initial state
    AlexEOF -> do
        let span = mkRealSrcSpan loc1 loc1
        setLastToken span 0
        return (L span ITeof)
    AlexError (AI loc2 buf) ->
        reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        let span = mkRealSrcSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes
        lt <- t span buf bytes
        case unRealSrcSpan lt of
          lt' -> do
            setLastTk lt'
            return lt

reportLexError :: RealSrcLoc -> RealSrcLoc -> StringBuffer -> [Char] -> P a
reportLexError loc1 loc2 buf str
  | atEnd buf = failLocMsgP loc1 loc2 (str ++ " at end of input")
  | otherwise =
  let c = fst (nextChar buf)
  in if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
     then failLocMsgP loc2 loc2 (str ++ " (UTF-8 decoding error)")
     else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)

lexTokenStream :: StringBuffer -> RealSrcLoc -> ParseResult [Located Token]
lexTokenStream buf loc = unP go initState
    where 
          initState = mkPState buf loc
          go = do
            ltok <- lexer False return
            case ltok of
              L _ ITeof -> return []
              _ -> liftM (ltok:) go

lexing str = case Lexer.lexTokenStream (StringBuffer.stringToStringBuffer str) (SrcLoc.mkRealSrcLoc (FastString.fsLit "") 0 0) of
  Lexer.POk _ a -> Prelude.map (\l -> case l of (SrcLoc.L _ e) -> e) a

getLineNumber (RealSrcSpan span) = srcSpanEndLine span
getCol (RealSrcSpan span) = srcSpanStartCol span

testLexing s = do
  inStr <- readFile s 
  print $ lexing inStr

}
