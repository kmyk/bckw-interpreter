#!/usr/bin/env runhaskell
module Main where
import Control.Monad.State
import Data.Char (chr, ord)
import System.IO
import System.Environment (getArgs, getProgName)

data BCKW
    = B0 | B1 BCKW | B2 BCKW BCKW
    | C0 | C1 BCKW | C2 BCKW BCKW
    | K0 | K1 BCKW
    | W0 | W1 BCKW
    | Ap BCKW BCKW
    | Z
    | S0 | S1 BCKW
    | Nil
    | Cons0 | Cons1 BCKW | Cons2 BCKW BCKW
    deriving (Eq, Ord, Show, Read)

alphabet :: String
alphabet = "BbCcKkWw`"

parse :: State String BCKW
parse = do
    let f (c : s) = (c, s)
        f _ = error "parse: unexpected EOF"
    c <- state f
    case c of
        c | c == 'B' || c == 'b' -> return B0
        c | c == 'C' || c == 'c' -> return C0
        c | c == 'K' || c == 'k' -> return K0
        c | c == 'W' || c == 'w' -> return W0
        c | c == '`' -> Ap <$> parse <*> parse

eval :: BCKW -> BCKW
eval (Ap  B0 x)       = B1 x
eval (Ap (B1 x) y)    = B2 x y
eval (Ap (B2 x  y) z) = eval (Ap x (Ap y z))
eval (Ap  C0 x)       = C1 x
eval (Ap (C1 x) y)    = C2 x y
eval (Ap (C2 x  y) z) = eval (Ap (Ap x z) y)
eval (Ap  K0 x)       = K1 x
eval (Ap (K1 x) y)    = eval x
eval (Ap  W0 x)       = W1 x
eval (Ap (W1 x) y)    = eval (Ap (Ap x y) y)
eval (Ap (Ap x y) z)  = case eval (Ap x y) of
    Ap x' y' -> Ap (Ap x' y') z
    xy -> eval (Ap xy z)
eval (Ap S0 x)        = S1 x
eval (Ap Cons0 x)     = Cons1 x
eval (Ap (Cons1 x) y) = Cons2 x y
eval f                = f

toScott :: Int -> BCKW
toScott 0 = K1 (C2 K0 K0)
toScott n = B2 K0 (C2 (C2 K0 K0) (toScott (n-1)))

fromScott :: BCKW -> Int
fromScott n = case eval (Ap (Ap n S0) Z) of
    Z -> 0
    S1 n' -> 1 + fromScott n'

toChurch :: String -> BCKW
toChurch "" = K1 (C2 K0 K0)
-- toChurch "" = toChurch (repeat (chr 256)) -- the Lazy K's way
toChurch (c : s) = W1 (B2 (C1 (B2 B0 (C2 (C2 K0 K0) (toScott (ord c))))) (toChurch s))

fromChurch :: BCKW -> String
fromChurch s = go (eval (Ap (Ap s Cons0) Nil)) where
    go Nil = ""
    -- go (Cons2 c s') | fromScott c >= 256 = ""
    go (Cons2 c s') = chr (fromScott c) : go (eval s')
    go _ = error "output: invalid list"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [ path ] -> do
            withFile path ReadMode $ \ fh -> do
                code <- hGetContents fh
                let (term, code') = runState parse (filter (`elem` alphabet) code)
                when (code' /= "") $ error ("parse: unexpected combinator: " ++ [ head code' ])
                input <- getContents
                let output = fromChurch (eval (Ap term (toChurch input)))
                putStr output
        _ -> do
            name <- getProgName
            error ("usage: " ++ name ++ " PATH")
