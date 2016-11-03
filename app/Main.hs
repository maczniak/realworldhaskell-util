module Main where

import Data.Char
import qualified Data.List as L
import Text.HTML.TagSoup

main :: IO ()
main = do
  content <- readFile "data-structures.html"
  let tags = parseTags content
  mapM_ putStr $ reverse $ fst $ foldl toMarkdown ([], [Bottom]) tags

data Keep = A String
          | Bottom
          | Code
          | Div (Maybe String)
          | Dl Int
          | Em
          | Header -- <h2>, <h3>
          | P
          | Pre
          | PreSH
          | Script
          | SpanCode
          | Table
          | TableExercise
          | Title
          | Ul
            deriving (Eq, Show)

toMarkdown :: ([String], [Keep]) -> Tag String -> ([String], [Keep])
toMarkdown acc@(list, queue) tag =
  case tag of
    TagOpen "a" attrs -> if head queue == Table
                         then (showTag tag:list, queue)
                         else case findAttr "href" attrs of
                              Just url -> ("[":anchor "name" attrs:list,
                                           A url:queue)
                              _        -> (anchor "name" attrs:list, queue)
    TagClose "a" -> let hd = safeHead queue
                    in case hd of
                         Just (A loc) ->
                            (("](" ++ normalizeURL loc ++ ")"):list, tail queue)
                         Just Table   -> ("</a>":list, queue)
                         _            -> acc
    TagOpen "b" _ -> ("<b>":list, queue)
    TagClose "b" -> if head list == "<b>" then (tail list, queue) -- empty body
                                          else ("</b>":list, queue)
    TagOpen "body" _ -> acc
    TagClose "body" -> acc
    TagOpen "br" _ -> ("<br>":list, queue)
    TagOpen "code" _ -> case safeHead queue of
                         Just Pre -> (list, Code:queue)
                         _        -> ("`":list, Code:queue)
    TagClose "code" -> case safeHead (tail queue) of
                        Just Pre -> (list, tail queue)
                        _ -> if last (head list) == '`'
                             -- when the latest tag was </em>
                             then (init (head list) : tail list, tail queue)
                             else ("`":list, tail queue)
    TagOpen "col" _ -> acc
    TagOpen "dd" _ -> acc
    TagClose "dd" -> acc
    TagOpen "div" attrs ->
                    (list, Div (findAttrExcept "class" attrs "titlepage"):queue)
    TagClose "div" -> (list, tail queue)
    TagOpen "dl" _ -> case safeHead queue of
                          Just (Dl depth) -> (list, Dl (depth + 1) : tail queue)
                          _               -> (list, Dl 0:queue)
    TagClose "dl" -> case head queue of
                       Dl 0 -> ("\n":list, tail queue)
                       Dl depth -> (list, Dl (depth - 1) : tail queue)
                       _ -> error "no dl in queue"
    TagOpen "dt" _ -> case head queue of
                          Dl depth -> ("* ":(replicate depth ' '):list, queue)
                          _ -> error "invalid depth"
    TagClose "dt" -> ("\n":list, queue)
    TagOpen "em" _ -> case safeHead queue of
                        Just Code -> ("`*":list, Em:queue)
                        _ -> ("*":list, Em:queue)
    TagClose "em" -> case safeHead (tail queue) of
                       Just Code -> ("*`":list, tail queue)
                       _ -> ("*":list, tail queue)
    TagOpen "h2" _ -> (header queue:list, Header:queue)
    TagClose "h2" -> ("\n\n":list, tail queue)
    TagOpen "h3" _ -> (header queue:list, Header:queue)
    TagClose "h3" -> ("\n\n":list, tail queue)
    TagOpen "head" _ -> acc
    TagClose "head" -> acc
    TagOpen "hr" attrs -> ("\n":showTag tag:list, queue)
    TagOpen "html" _ -> acc
    TagClose "html" -> acc
    TagOpen "img" attrs -> case findAttr "src" attrs of
                             Just src -> (("![](" ++ src ++ ")"):list, queue)
                             _ -> error "no src in img"
    TagOpen "li" _ -> ("* ":list, queue)
    TagClose "li" -> ("\n":list, queue)
    TagOpen "link" _ -> acc
    TagOpen "meta" _ -> acc
    TagOpen "p" _ -> (list, P:queue)
    TagClose "p" -> if elem (head (tail queue)) [TableExercise, Ul]
                    then (list, tail queue)
                    else if last (head list) == '\n'
                         then ("\n":list, tail queue) -- when <p> ends with "\n"
                         else ("\n\n":list, tail queue)
    TagOpen "pre" attrs -> if findAttr "class" attrs == Just "programlisting"
                           then ("\n```haskell\n":anchor "name" attrs:list,
                                 PreSH:queue)
                           else ("<pre>":anchor "name" attrs:list, Pre:queue)
    TagClose "pre" -> case head queue of
                        Pre -> ("</pre>\n\n":list, tail queue)
                        PreSH -> ("\n```\n\n":list, tail queue)
                        _ -> error "unmatched <pre>"
    TagOpen "script" _ -> (list, Script:queue)
    TagClose "script" -> (list, tail queue)
    TagOpen "span" attrs -> case findAttr "class" attrs of
                                Just "authors" -> (" ":list, queue)
                                Just "type" -> ("`":list, SpanCode:queue)
                                _              -> acc
    TagClose "span" -> case safeHead queue of
                         Just SpanCode -> ("`":list, tail queue)
                         _ -> acc
    TagOpen "strong" _ -> ("**":list, queue)
    TagClose "strong" -> ("**":list, queue)
    TagOpen "sup" _ -> ("<sup>":list, queue)
    TagClose "sup" -> ("</sup>":list, queue)
    TagOpen "table" attrs -> if isExercise queue
                             then (list, TableExercise:queue)
                             else (showTag tag:list, Table:queue)
    TagClose "table" -> case head queue of
                          TableExercise -> ("\n":list, tail queue)
                          _ -> ("</table>\n\n":list, tail queue)
    TagOpen "tbody" _ -> acc
    TagClose "tbody" -> acc
    TagOpen "td" attrs -> case head queue of
                            TableExercise -> acc
                            _ -> (showTag tag:list, queue)
    TagClose "td" -> case head queue of
                       TableExercise -> acc
                       _ -> ("</td>":list, queue)
    TagOpen "th" attrs -> (showTag tag:list, queue)
    TagClose "th" -> ("</th>":list, queue)
    TagOpen "title" _ -> (list, Title:queue)
    TagClose "title" -> (list, tail queue)
    TagOpen "tr" attrs -> case head queue of
                            TableExercise -> ("1. ":list, queue)
                            _ -> (showTag tag:list, queue)
    TagClose "tr" -> case head queue of
                       TableExercise -> ("\n":list, queue)
                       _ -> ("</tr>":list, queue)
    TagOpen "ul" _ -> (list, Ul:queue)
    TagClose "ul" -> ("\n":list, tail queue)
    TagText txt -> case safeHead queue of
                        Just (A _) -> (stripText txt:list, queue)
                        Just Bottom -> acc
                        Just Code -> (stripCode txt:list, queue)
                        Just Em -> (txt:list, queue)
                        Just Header -> (txt:list, queue)
                        Just P -> if isExerciseNumber txt queue
                                  then acc
                                  else (stripText txt:list, queue)
                        Just PreSH -> (strip txt:list, queue)
                        Just Pre -> (txt:list, queue)
                        Just Script -> acc
                        Just SpanCode -> (txt:list, queue)
                        Just Table -> (txt:list, queue)
                        Just Title -> acc
                        Just missing -> (show missing:"#####":list, queue)
                        Nothing -> ("#####Nothing":list, queue)
    missing -> (show missing:"######":list, queue)

safeHead []    = Nothing
safeHead (x:_) = Just x

findAttr = lookup
findAttrExcept attrName xs exVal = let val = findAttr attrName xs
                                   in if val == Just exVal then Nothing else val

-- TODO: implement a Show instance
showTag (TagOpen tagName attrs) = showTagWithAttrs tagName attrs
showTagWithAttrs name attrs =
  let attrsText = L.intercalate " " $ pairs attrs
      space = if null attrsText then "" else " "
  in L.intercalate "" ["<", name, space, attrsText, ">"]
  where pairs (x:xs) = L.intercalate ""
                                     [fst x, "=\"", snd x, "\""]
                       : pairs xs
        pairs [] = []

anchor attrName attrs = case findAttr attrName attrs of
                          Just name -> "<a name=\"" ++ name ++ "\"></a>"
                          _         -> ""

latestClass (x:xs) = case x of
                       Div (Just className) -> Just className
                       Div _ -> latestClass xs
                       _ -> Nothing
latestClass [] = Nothing

header queue =
  case latestClass queue of
    Just "navheader" -> "# " -- book title
    Just "chapter" -> "# "
    Just "sect1" -> "## "
    Just "sect2" -> "### " -- in <h3>
    Just className -> error $ "unknown class " ++ className
    Nothing -> error "nothing class"

isExercise queue = latestClass queue == Just "qandaset"

isExerciseNumber txt queue = length txt == 2
                             && isNumber (txt !! 0)
                             && txt !! 1 == '.'
                             && safeHead (tail queue) == Just TableExercise

normalizeURL url =
  if url == "/feeds/comments/" then "http://book.realworldhaskell.org" ++ url
                               else url

stripText txt = L.intercalate "\n" $ map stripIndent $ lines txt
  where stripIndent s =
          if (length $ takeWhile isSpace s) == 1 && s !! 0 /= '\t'
          then s
          else stripStart s

stripCode txt = L.intercalate "\n" $ map stripIndent $ lines txt
  where stripIndent s =
          let s' = stripStart s
              first = safeHead s'
          in case first of
                  Just '+' -> replicate 4 ' ' ++ s'
                  Just '*' -> replicate 4 ' ' ++ s'
                  _ -> s'

strip = stripStart . stripEnd
stripStart = dropWhile isSpace
stripEnd = L.dropWhileEnd isSpace

