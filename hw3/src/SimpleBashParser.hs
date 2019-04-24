{-# LANGUAGE LambdaCase #-}

module SimpleBashParser
  ( parseCommand
  , parseScript
  ) where

import           Control.Monad.Combinators (skipMany, skipSome)

import qualified Data.Text as T
import           Data.Text.IO (hGetContents)

import           System.IO (IOMode (..), openFile)

import           Text.Megaparsec (anySingleBut, errorBundlePretty, many, noneOf,
                                  oneOf, parse, some, try, (<|>))
import           Text.Megaparsec.Char (alphaNumChar, char, digitChar, string)

import           Structure (Command (..), Parser, TemplateArg (..), Value (..))

varName :: Parser T.Text
varName = T.pack <$> (some digitChar <|> some (alphaNumChar <|> char '_'))

slash :: Parser Char
slash = char '\\' *> (char '\\' <|> char '$' <|> char '"')

textChar :: Parser Char
textChar = noneOf "\t\n\r ;$'\"\\()"

textSlashChar :: Parser Char
textSlashChar = textChar <|> slash

argText :: Parser Value
argText = Text <$> (T.pack <$> some textSlashChar)

argVar :: Parser Value
argVar = Variable <$> (char '$' *> try varName)

argQuotes :: Parser Value
argQuotes = Text <$> (char '\'' *> (T.pack <$> many (anySingleBut '\'')) <* char '\'')

stringT :: String -> Parser T.Text
stringT s = string (T.pack s)

subShellParser :: Parser [Command]
subShellParser = (stringT "$(" *> manyWhiteSpaces
  *> commands
  <* (manyWhiteSpaces *> char ')'))

subShell :: Parser Value
subShell = SubShell <$> subShellParser

subShellCom :: Parser Command
subShellCom = SubShellCommand <$> subShellParser

emptyString :: Parser T.Text
emptyString = stringT ""

argTemplate :: Parser Value
argTemplate = Template <$>
  (char '"' *>
  (many ((char '$' *> ((TemplateVar <$> varName)
  <|> (TemplateShell <$> (char '(' *> commands <* char ')'))
  <|> (TemplateText . const (T.pack "$") <$> emptyString)))
  <|> (TemplateText <$> (T.pack <$> some (try slash <|> noneOf "\"$")))))
  <* char '"')

oneArgument :: Parser Value
oneArgument = argQuotes <|> try argVar <|> subShell <|> argTemplate <|> argText

composeArgument :: Parser [Value]
composeArgument = some oneArgument

assignment :: Parser Command
assignment = Assignment <$> varName <*> (char '=' *> composeArgument)

manySpaces :: Parser ()
manySpaces = skipMany (char ' ')

someSpaces :: Parser ()
someSpaces = skipSome (char ' ')

manyWhiteSpaces :: Parser ()
manyWhiteSpaces = skipMany (oneOf "\t\n\r ")

someWhiteSpaces :: Parser ()
someWhiteSpaces = skipSome (oneOf "\t\n\r ")

echo :: Parser Command
echo = (const Echo <$> (stringT "echo"))
  <*> (try (const False <$> (someSpaces *> stringT "-n"))
  <|> (const True <$> emptyString))
  <*> (many (try (someSpaces *> composeArgument)))

exit :: Parser Command
exit = Exit <$> (stringT "exit" <* someSpaces *> (T.pack <$> some digitChar))

pwd :: Parser Command
pwd = const PWD <$> (stringT "pwd")

cd :: Parser Command
cd = CD <$> (stringT "cd" *> someSpaces *> composeArgument)

readArg :: Parser Command
readArg = (const Read <$> (stringT "read")) <*> (many $ try (someSpaces *> varName))

innerCommands :: Parser Command
innerCommands = echo <|> exit <|> pwd <|> cd <|> readArg

unknownCommand :: Parser Command
unknownCommand = (Unknown <$> composeArgument) <*> (many $ try (someSpaces *> composeArgument))

separator :: Parser ()
separator = manySpaces <* char ';'

commandBeforeWord :: String -> Parser [Command]
commandBeforeWord word = (mempty <$> try (stringT word))
  <|> (pure <$> try (internalAndExternal <* separator <* manyWhiteSpaces))
  <|> ((\x -> [x, x]) <$> (try (internalAndExternal
  <* manyWhiteSpaces <* stringT word)))

sepCommandsBeforeWord :: String -> [Command] -> Parser [Command]
sepCommandsBeforeWord word z = commandBeforeWord word >>= \case
  []     -> return z
  (x:[]) -> sepCommandsBeforeWord word (z ++ [x])
  (x:_)  -> return (z ++ [x])

commandBeforeElseFi :: Parser ([Command], String)
commandBeforeElseFi = ((\x -> ([], T.unpack x)) <$> (try (stringT "else") <|> try (stringT "fi")))
  <|> ((\x -> ([x], "")) <$> try (internalAndExternal
  <* (try separator <|> someWhiteSpaces) <* manyWhiteSpaces))

commandBeforeW :: String -> Parser [Command]
commandBeforeW word = (mempty <$> try (stringT word))
  <|> (pure <$> try (internalAndExternal <*
  (try separator <|> someWhiteSpaces) <* manyWhiteSpaces))

commandInIfBody :: [Command] -> ([Command] -> [Command] -> Command) -> Parser Command
commandInIfBody z construct = commandBeforeElseFi >>= \case
  ([], w) -> if (w == "else")
               then someWhiteSpaces *> commandInElseBody [] (construct z)
               else return (construct z [])
  (x, _)  -> commandInIfBody (z ++ x) construct

commandInElseBody :: [Command] -> ([Command] -> Command) -> Parser Command
commandInElseBody z construct = commandBeforeW "fi" >>= \case
  [] -> return (construct z)
  x  -> commandInElseBody (z ++ x) construct

ifStatement :: Parser Command
ifStatement = ((const IfThen <$> (stringT "if"))
  <*> (someWhiteSpaces *> sepCommandsBeforeWord "then" [] <* someWhiteSpaces))
  >>= commandInIfBody []

commandInWhileBody :: [Command] -> ([Command] -> Command) -> Parser Command
commandInWhileBody z construct = commandBeforeW "done" >>= \case
  [] -> return (construct z)
  x  -> commandInWhileBody (z ++ x) construct

whileStatement :: Parser Command
whileStatement = ((const While <$> (stringT "while"))
  <*> (someWhiteSpaces *> sepCommandsBeforeWord "do" [] <* manyWhiteSpaces))
  >>= commandInWhileBody []

command :: Parser Command
command = try subShellCom <|> try assignment <|> try innerCommands
  <|> try ifStatement <|> try whileStatement

commandSeparator :: Parser ()
commandSeparator = manyWhiteSpaces *> (stringT ";" <|> emptyString) *> manyWhiteSpaces

internalAndExternal :: Parser Command
internalAndExternal = try command <|> unknownCommand

commands :: Parser [Command]
commands = manyWhiteSpaces *> many (internalAndExternal <* commandSeparator)

parseScript :: FilePath -> IO [Command]
parseScript script = do
  text <- openFile script ReadMode >>= hGetContents
  case parse commands script text of
    Left bundle -> fail (errorBundlePretty bundle)
    Right cmnds -> return cmnds

parseCommand :: T.Text -> IO Command
parseCommand text =
  case parse internalAndExternal "" text of
    Left bundle -> fail (errorBundlePretty bundle)
    Right cmnd -> return cmnd
