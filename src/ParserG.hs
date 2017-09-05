module ParserG where

import Text.ParserCombinators.Parsec
import LanguageCore
import DrawShapes
import Graphics.Gloss
import qualified Control.Applicative as A
import Control.Lens.Extras

runTurtleParser :: String -> [Commands]
runTurtleParser (parse languageParser "" -> Right x)  = x
runTurtleParser (parse languageParser "" -> Left err) = error $ show err

languageParser :: Parser [Commands]
languageParser = do
  optional (many1 space)
  x <- many (    command1Parser
             <|> command2Parser
             <|> command3Parser
             <|> command4Parser
             <|> command5Parser
             <|> commentsParser
             <|> ifStatment
             <|> stopParser
             <|> try functionArgsParser
             <|> functionParser
             <|> try funcArgGetParser
             <|> funcGetParser
             )
  return x

commentsParser :: Parser Commands
commentsParser = do
  string "**"
  manyTill anyToken newline
  optional (many (newline <|> space))
  return $ Comment

functionParser :: Parser Commands
functionParser = do
  string "TO" >> space
  funcName <- manyTill anyChar space
  optional (many (newline <|> space))
  commands <- languageParser
  optional (many (newline <|> space))
  string "END"
  notFollowedBy letter
  optional (many (newline))
  return (FuncSetArg funcName [] commands)

functionArgsParser :: Parser Commands
functionArgsParser = do
  string "TO" >> space
  funcName <- manyTill anyChar space
  args <- many1 argsParser
  optional (many (newline <|> space))
  commands <- languageParser
  optional (many (newline <|> space))
  string "END"
  notFollowedBy letter
  optional (many (newline <|> space))
  return (FuncSetArg funcName (map toArgs args) commands)
    where toArgs (Var x) = x

argsParser :: Parser Commands
argsParser = do
  char ':'
  arg <- many1 alphaNum
  optional (many space)
  return (Var (':':arg))

funcGetParser :: Parser Commands
funcGetParser = do
  varName <- (many1 lower)
  skipMany space
  notFollowedBy digit
  optional (many (newline <|> space))
  return (FuncGetArg varName [LitFs []])

funcArgGetParser :: Parser Commands
funcArgGetParser = do
  varName <- try (manyTill lower space)
  skipMany space
  args <- many1 funcArgsParse
  optional (many (newline <|> space))
  return $ FuncGetArg varName args

funcArgsParse :: Parser Commands
funcArgsParse = do
  args <- (try arithOperations <|> try floatParser <|> numberParser <|> argsParser)
  optional (many space)
  return args

command1Parser :: Parser Commands
command1Parser = do
  name <- string "Backward" <|> string "Forward"
  skipMany1 space
  value <- (try arithOperations <|> try floatParser <|> numberParser <|> argsParser) <?> "you forgot to write a number here"
  optional (space <|> (char ',' >> space) <|> newline)
  optional (many space)
  case name of
    "Backward" -> return $ Backward value
    "Forward"  -> return $ Forward value

command2Parser :: Parser Commands
command2Parser = do
  name <- try (string "Penup")  <|> (string "Pendown")
  notFollowedBy alphaNum
  optional (space <|> (char ',' >> space) <|> newline)
  optional (many space)
  return (read name :: Commands)


stopParser :: Parser Commands
stopParser = do
  stop <- try (string "Stop")
  notFollowedBy alphaNum
  optional (space <|> (char ',' >> space) <|> newline)
  optional (many space)
  return $ Stop

command3Parser :: Parser Commands
command3Parser = do
  name <- try (string "Right") <|> string "Left"
  case name of
    "Right" -> do
      skipMany1 space
      value <-  (try arithOperations <|> try floatParser <|> numberParser <|> argsParser) <?> "you forgot to write a number here"
      optional (space <|> (char ',' >> space) <|> newline)
      optional (many space)
      return (TurnR value)
    "Left" -> do
      skipMany1 space
      value <- (try arithOperations <|> try floatParser <|> numberParser <|> argsParser) <?> "you forgot to write a number here"
      optional (space <|> (char ',' >> space) <|> newline)
      optional (many space)
      return (TurnL value)

command4Parser :: Parser Commands
command4Parser = do
  name <- string "Color"
  optional $ skipMany1 space
  color <- many1 letter <?> "you forgot to write the name of the color"
  notFollowedBy alphaNum
  optional (space <|> (char ',' >> space) <|> newline)
  optional (many space)
  case color of
    "red"    -> return (Colors Red)
    "blue"   -> return (Colors Blue)
    "black"  -> return (Colors Black)
    "white"  -> return (Colors White)
    "green"  -> return (Colors Green)
    "yellow" -> return (Colors Yellow)
    "cyan"   -> return (Colors Cyan)
    otherwise -> unexpected "invalid color name or this color is not available" <?>
                            "color name: black, white, red, green, yellow, cyan and blue"

command5Parser :: Parser Commands
command5Parser = do
  name <- string "Repeat"
  skipMany1 space
  howMany <- (try arithOperations <|> numberParser <|> argsParser)
  skipMany space
  char '['
  optional $ skipMany1 space
  commands <- languageParser
  optional $ skipMany1 space
  char ']'
  optional (space <|> (char ',' >> space) <|> newline)
  optional (many space)
  return (Repeat howMany commands)

ifStatment :: Parser Commands
ifStatment = do
  string "If"
  skipMany space
  expr <- eqOperations
  skipMany space
  char '['
  optional $ skipMany1 space
  commands <- languageParser
  optional $ skipMany1 space
  char ']'
  optional (space <|> (char ',' >> space) <|> newline)
  optional (many space)
  return (IF expr commands)

arithOperations :: Parser Commands
arithOperations = do
  arg1 <- (try floatParser <|> numberParser <|> argsParser)
  optional space
  op <- many1 $ oneOf "+*-/"
  optional (many space)
  arg2 <- try (arithOperations) <|> (try floatParser <|> numberParser <|> argsParser)
  return $ OpV (matchAr op arg1 arg2)

eqOperations :: Parser Commands
eqOperations = do
  arg1 <- try arithOperations <|> (try floatParser <|> numberParser <|> argsParser)
  optional space
  op <- try (string ">=" <|> string  "<=") <|> string "==" <|> string "!=" <|>  string ">" <|>  string "<"
  optional (many space)
  arg2 <- try arithOperations <|> (try floatParser <|> numberParser <|> argsParser)
  optional (many space)
  return $ OpV (matchOrd op arg1 arg2)

matchAr :: String -> Commands -> Commands -> Commands
matchAr "+" x y = Add  x y
matchAr "-" x y = Sub  x y
matchAr "*" x y = Mult x y
matchAr "/" x y = Div  x y

matchOrd :: String -> Commands -> Commands -> Commands
matchOrd ">" x y = BT   x y
matchOrd "<" x y = ST   x y
matchOrd ">=" x y = BEQ   x y
matchOrd "<=" x y = SEQ   x y
matchOrd "==" x y = Eq   x y
matchOrd "!=" x y = NOT   x y

numberParser :: Parser Commands
numberParser = do
  number <- many1 digit
  return $ LitF (read number :: Float)

floatParser :: Parser Commands
floatParser = do
  vorComma <- many1 digit
  char '.'
  nachComma <- many1 digit
  return $ LitF (read (vorComma ++ "." ++ nachComma) :: Float )
