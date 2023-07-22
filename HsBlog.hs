data MarkdownElem =
    Text String
    | Heading Int String
    | Paragraph [MarkdownElem]
    | Bold MarkdownElem
    | Italic MarkdownElem
    | BoldAndItalic MarkdownElem
    | Blockquote MarkdownElem
    | OrderedList [MarkdownElem]
    | UnorderedList [MarkdownElem]
    | Code MarkdownElem
    | CodeBlock [MarkdownElem]
    | HorizontalRule
    | Link MarkdownElem String
    | LinkTitle MarkdownElem String String
    | Image String String String
    deriving (Show)


data Markdown = Markdown [MarkdownElem]

data MarkdownContext =
    Empty
    | ParagraphC [MarkdownElem]

after :: Char -> String -> String
after c (x:xs) = if c == x then after c xs else x : xs
after _ [] = []

ltrim :: String -> String
ltrim = after ' '

parseMd :: String -> Markdown
parseMd = Markdown $ reverse $ parseMdState Empty (lines text) []

parseMdState :: MarkdownContext -> [String] -> [MarkdownElem] -> [MarkdownElem]
parseMdState Empty (('#':line):text) parsed = parseMdState Empty text ((Heading trimmed number) : parsed) where
    trimmed = ltrim $ after '#' line
    number = count (\c -> c == '#') line + 1
parseMdState Empty ("":text) parsed = parseMdState Empty text parsed
parseMdState Empty (line:text) parsed = parseMdState (ParagraphC [Text line]) text parsed
parseMdState (ParagraphC p) ("":text) parsed = parseMdState Empty text ((Paragraph p) : parsed)
parseMdState (ParagraphC p) (line:text) parsed = parseMdState (ParagraphC (p ++ [Text line])) text parsed
parseMdState (ParagraphC p) [] parsed =  (Paragraph p) : parsed
parseMdState _ [] parsed = parsed

renderAsHtml :: Markdown -> String
renderAsHtml = undefined

convert :: String -> String
convert = renderHtml . parseMd

main :: IO ()
main = do
    --filename <- getLine
    let filename = "example.md" -- TODO: remove later
    text <- readFile filename
    let md = parseMd text -- TODO: remove later
    print md -- TODO: remove later
    --let res = convert text
    --putStrLn res


