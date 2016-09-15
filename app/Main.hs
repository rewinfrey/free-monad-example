{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Free.Church
-- import Control.Monad.Free
import Data.Foldable
import Lib
import Control.Monad

data CommandF next =
    PutLine String next
  | GetLine (String -> next)
  deriving (Functor)

type Command = F CommandF

gtLine :: (MonadFree CommandF m) => m String
gtLine = wrap (GetLine return)

putLine :: (MonadFree CommandF m) => String -> m ()
putLine message = wrap (PutLine message (return ()))

run :: Command a -> IO a
run = iterM (\command -> case command of
  PutLine message next -> do
    putStrLn message
    next
  GetLine next -> do
    input <- Prelude.getLine
    next input
  )

app :: Command ()
app = do
  putLine "What's your name?"
  input <- gtLine
  putLine input
  putLine "What's your age?"
  input2 <- gtLine
  putLine input2
  putLine "Thank you"

runApp = run

main :: IO ()
main = do
  runApp app
