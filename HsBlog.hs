import GHC.Utils.Misc (count)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Pos

import Data.Functor.Identity

data MarkdownElem =
    Text String
    | Headings [(Int, String)]
    | Heading Int String --
    | Paragraph [MarkdownElem] --
    | Bold MarkdownElem
    | Italic MarkdownElem
    | BoldAndItalic MarkdownElem
    | Strikethrough MarkdownElem
    | Blockquote [MarkdownElem] --
    | OrderedList [MarkdownElem] --
    | UnorderedList [MarkdownElem] --
    | Code MarkdownElem
    | CodeBlock [MarkdownElem] --
    | HorizontalRule
    | Link MarkdownElem String
    | LinkTitle MarkdownElem String String
    | Image String String String
    deriving (Show)


data Markdown = Markdown [MarkdownElem]
    deriving (Show)

data MarkdownContext =
    EmptyC
    | ParagraphC [MarkdownElem]
    | BlockquoteC

type ParseResult = Either ParseError MarkdownElem

after :: Char -> String -> String
after c (x:xs) = if c == x then after c xs else x : xs
after _ [] = []

ltrim :: String -> String
ltrim = after ' '

connect :: MarkdownElem -> MarkdownElem -> ParseResult
connect a (Text "") = Right a
connect (Text a) (Text b) = Right $ Text (a ++ b)
connect (Heading n a) (Text b) = Right $ Heading n (a ++ b)
connect (Heading n a) (Heading m b) = Right $ Headings [(n, a), (m, b)]
connect (Headings xs) (Heading m b) = Right $ Headings (xs ++ [(m, b)])
connect (Heading n a) (Headings xs) = Right $ Headings ([(n, a)] ++ xs)
connect (Blockquote a) (Text b) = Right $ Blockquote (a ++ [Text b])
connect (Blockquote a) (Blockquote b) = Right $ Blockquote (a ++ b)
connect (OrderedList a) (Text b) = Right $ OrderedList (a ++ [Text b])
connect (OrderedList a) (OrderedList b) = Right $ OrderedList (a ++ b)
connect (UnorderedList a) (Text b) = Right $ UnorderedList (a ++ [Text b])
connect (UnorderedList a) (UnorderedList b) = Right $ UnorderedList (a ++ b)
connect (CodeBlock a) (Text b) = Right $ CodeBlock (a ++ [Text b])
connect (CodeBlock a) (CodeBlock b) = Right $ CodeBlock (a ++ b)
connect (Paragraph a) (Text b) = Right $ Paragraph (a ++ [Text b])
connect (Paragraph a) (Paragraph b) = Right $ Paragraph (a ++ b)
connect (Text a) (b) = connect b (Text a)
connect a b = Left $ newErrorMessage (Message msg) (initialPos "Inside") where
    msg = "Error when connecting parsed elements: " ++ (show a) ++ " : " ++ (show b)


connectE :: ParseResult -> ParseResult -> ParseResult
connectE (Left a) _ = Left a
connectE _ (Left a) = Left a
connectE (Right a) (Right b) = connect a b

headingParser :: Parser MarkdownElem
headingParser = do
    hashes <- many1 $ char '#'
    spaces
    name <- many anyChar
    pure $ Heading (length hashes) name

allParser :: Parsec String MarkdownContext MarkdownElem
allParser = do
    char '>'
    spaces
    text <- many anyChar
    putState BlockquoteC
    pure $ Blockquote [Text text]

paragraphParser :: Parser MarkdownElem
paragraphParser = do
    text <- many anyChar
    pure $ Paragraph $ map Text $ lines text

blocks :: [String] -> [[String]]
blocks texts = filter (not . null) $ reverse $ map reverse $ blocksRec [] texts

blocksRec :: [[String]] -> [String] -> [[String]]
blocksRec xs [] = xs
blocksRec xs ("":texts) = blocksRec ([] : xs) texts
blocksRec (x:xs) (text:texts) = blocksRec ((text : x) : xs) texts
blocksRec [] (text:texts) = blocksRec ([[text]]) texts

parseMd :: String -> Either ParseError Markdown
parseMd text = fmap Markdown (sequence $ map parseBlock (blocks $ lines text))

parseBlock :: [String] -> ParseResult
parseBlock = (foldr connectE (Right (Text ""))) . (map parseLine) -- 

--parseMdState :: MarkdownContext -> [[String]] -> [MarkdownElem] -> [MarkdownElem]
--parseMdState = undefined
--parseMdState EmptyC (('#':line):text) parsed = parseMdState EmptyC text ((Heading number trimmed) : parsed) where -- TODO: rewrite using Parsec
--    trimmed = ltrim $ after '#' line
--    number = count (\c -> c == '#') line + 1
--parseMdState EmptyC ("":text) parsed = parseMdState EmptyC text parsed
--parseMdState EmptyC (line:text) parsed = parseMdState (ParagraphC [parseLine line]) text parsed
--parseMdState (ParagraphC p) ("":text) parsed = parseMdState EmptyC text ((Paragraph p) : parsed)
--parseMdState (ParagraphC p) (line:text) parsed = parseMdState (ParagraphC (p ++ [parseLine line])) text parsed
--parseMdState (ParagraphC p) [] parsed = (Paragraph p) : parsed
--parseMdState _ [] parsed = parsed

parseLine :: String -> ParseResult
parseLine = parse ((try headingParser) <|> paragraphParser) ""

renderAsHtml :: Markdown -> String
renderAsHtml (Markdown md) = concat $ map renderElem md

el :: String -> String -> String
el e text = "<" ++ e ++ ">" ++ text ++ "</" ++ e ++ ">"

renderHeading :: (Int, String) -> String
renderHeading (n, text) = el ("h" ++ num) text where num = show n

renderElem :: MarkdownElem -> String
renderElem (Text text) = text
renderElem (Heading n text) = el ("h" ++ num) text where num = show n
renderElem (Headings xs) = concat (map renderHeading xs)
renderElem (Paragraph xs) = el "p" $ concat $ map renderElem xs
renderElem (Bold element) = el "b" $ renderElem element
renderElem (Italic element) = el "em" $ renderElem element
renderElem (BoldAndItalic element) = el "em" $ el "b" $ renderElem element
renderElem (Strikethrough element) = el "s" $ renderElem element

convert :: String -> Either ParseError String
convert text = renderAsHtml <$> parseMd text

main :: IO ()
main = do
    --filename <- getLine
    let filename = "example copy.md" -- TODO: remove later
    text <- readFile filename
    let md = parseMd text -- TODO: remove later
    --print md -- TODO: remove later
    let res = convert text
    print res


