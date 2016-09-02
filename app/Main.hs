{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.Free
import Data.Foldable
import Lib

data Provision next =
  Begin next |
  Install String next |
  IfNotExists String (Free Provision ()) next |
  Touch String next |
  Cd String next |
  MkDir String Bool next |
  Echo String next |
  Continue |
  Done
  deriving(Functor)

begin = liftF $ Begin id

install what = liftF $ Install what id

ifNotExists path what = liftF $ IfNotExists path what id

touch path = liftF $ Touch path id

cd path = liftF $ Cd path id

mkDir path wholeTree = liftF $ MkDir path wholeTree id

echo message = liftF $ Echo message id

continue = liftF $ Continue

done = liftF Done

-- class InterpretingContext a where
--   run :: Free Provision () -> a
--
-- instance InterpretingContext String where
--   run (Free (Begin next)) =
--     "#!/usr/bin/env bash\n\n" ++ (run next)
--
--   run (Free (Install what next)) =
--     "apt-get install " ++ what ++ "\n" ++ nextStr
--     where
--       nextStr = run next
--
--   run (Free (IfNotExists path what next)) =
--     "if [ ! -f " ++ path ++ " ]; then\n\t" ++ whatStr
--       ++ "\nfi\n" ++ nextStr
--     where
--       whatStr = run what
--       nextStr = run next
--
--   run (Free (Touch path next)) =
--     "touch " ++ path ++ "\n" ++ (run next)
--
--   run (Free (Cd path next)) =
--     "cd " ++ path ++ "\n" ++ (run next)
--
--   run (Free (MkDir path tree next)) =
--     "mkdir " ++ treeOption ++ " " ++ path ++ "\n" ++ (run next)
--     where
--       treeOption =
--         if tree then "-p" else ""
--
--   run (Free (Echo message next)) =
--     "echo " ++ message ++ "\n" ++ run next
--
--   run (Free Continue) = ""
--
--   run (Free Done) = "exit 0"

run (Free (Begin next)) =
  "#!/usr/bin/env bash\n\n" ++ (run next)

run (Free (Install what next)) =
  "apt-get install " ++ what ++ "\n" ++ nextStr
  where
    nextStr = run next

run (Free (IfNotExists path what next)) =
  "if [ ! -f " ++ path ++ " ]; then\n\t" ++ whatStr
    ++ "\nfi\n" ++ nextStr
  where
    whatStr = run what
    nextStr = run next

run (Free (Touch path next)) =
  "touch " ++ path ++ "\n" ++ (run next)

run (Free (Cd path next)) =
  "cd " ++ path ++ "\n" ++ (run next)

run (Free (MkDir path tree next)) =
  "mkdir " ++ treeOption ++ " " ++ path ++ "\n" ++ (run next)
  where
    treeOption =
      if tree then "-p" else ""

run (Free (Echo message next)) =
  "echo " ++ message ++ "\n" ++ run next

run (Free Continue) = ""

run (Free Done) = "exit 0"

app :: Free Provision ()
app = do
  begin
  install "postgresql-server"
  mkDir "/var/run/the-app" True
  cd "/var/run/the-app"
  ifNotExists "the-app.log" $ touch "the-app.log" >> continue
  done

runApp :: Free Provision () -> String
runApp = run

main :: IO String
main = do
  return $ runApp app
