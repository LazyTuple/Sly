
module Parse where

import Text.ParserCombinators.ReadP
import Data.Char

type Line = (Int, String)

data Term
   = Str String
   | Dbl Double
   | Num Int
   | Ide String
   | Res Char
   deriving (Eq,Read,Show)

reserved = [ '{', '}', '(', ')', '=' ]

readIden = let
   p c = (not $ isSpace c) && (all (c /=) reserved)
   in munch p >>= return.Ide

readTerm =
   (readS_to_P reads >>= return.Str) <++
   (readS_to_P reads >>= return.Num) <++
   (readS_to_P reads >>= return.Dbl) <++
   ((satisfy $ \c -> any (c ==) reserved) >>= return.Res) <++
   readIden

readToken total = do
   munch isSpace
   col <- look >>= return . (total - ) . length
   trm <- readTerm
   return (col,trm)
   
readLine :: ReadP [(Int, Term)]
readLine = do
   tot <- look >>= return . length
   r <- many $ readToken tot
   eof
   return r
   
numLines ls = zip [1 .. length ls] (lines ls)

termLines ls = let
   g ln = case readP_to_S readLine ln of
      ((a,_):_) -> a ; _ -> []
   f (n, ln) = (n, g ln)
   in map f $ numLines ls

tokenLines ls = let
   g lin (col, trm) = (lin, col, trm)
   f (n , ts) = map (g n) ts
   in map f $ termLines ls

type Token = (Int, Int, Term)

data Block
   = Span [Token]
   | Chunk Char [Block]
   deriving (Eq,Read,Show)

increase a bs = Right $ (Nothing , (Chunk a [])) : bs

decrease ((_ , a) : (n , Chunk b cs) : ds) =
   Right $ (n , Chunk b (cs ++ [a])) : ds
decrease _ = Left "pattern error decreasing indent"

indent n x@((Just m, as) : bs) = case m > n of
   False -> decrease x >>= indent n
   True -> case n == m of
      True -> Right $ (Just m, as) : bs
      False -> Left $ "indentation error near: \n" ++ show as
indent m ((Nothing, as) : bs) = Right $ (Just m, as) : bs
indent _ _ = Left $ "indentation error..."

{-
margin n m st@((Just m', a) : bs) = case m > m' of
   False -> decrease st >>= margin n m
   True -> Left $ "indentation error in line: " ++ show n
margin n m ((Nothing, a) : bs)  = return $ (Just m, a) : bs
-}

-- data Blk 

a -+- b = a ++ "\n" ++ b

src0 = "a = b" -+- " c" -+- " 1 { 3.0"
