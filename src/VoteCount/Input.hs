module VoteCount.Input where

import qualified Data.Map as M
import Data.List
import Control.Monad

import VoteCount.Count (Ballot, Count)

type CandidateInfo = M.Map Char String

getCandidates :: IO CandidateInfo
getCandidates = getCandidateOrQuit M.empty

getCandidateOrQuit :: CandidateInfo -> IO CandidateInfo
getCandidateOrQuit currentCandidates = do
  putStrLn "\nPlease enter the name of a candidate in this election (or press enter to finish entering candidates)"
  response <- getLine
  case response of
    "" -> return currentCandidates
    newCandidate -> do
      initial <- askForInitial newCandidate (M.keys currentCandidates)
      getCandidateOrQuit $ M.insert initial newCandidate currentCandidates

askForInitial :: String -> [Char] -> IO Char
askForInitial name takenChars = do
  putStrLn $ "Please enter the initial for the candidate \"" ++ name ++ "\""
  response <- getLine
  if length response /= 1
    then do
    putStrLn "Response should be a character"
    askForInitial name takenChars
    else (if head response `elem` takenChars
            then do
             putStrLn "This initial is already taken!"
             askForInitial name takenChars
          else return $ head response)

getBallots :: CandidateInfo -> IO [Ballot]
getBallots = flip getBallotOrQuit []

getBallotOrQuit :: CandidateInfo -> [Ballot] -> IO [Ballot]
getBallotOrQuit candidates ballots = do
  putStrLn "Please enter a ballot of the candidates as a string of the initials (in order of preferences) with no spaces. Press enter to stop entering ballots."
  ballot <- getLine
  case ballot of
    "" -> return ballots
    newBallot ->
      if validateBallot (M.keys candidates) newBallot
      then getBallotOrQuit candidates $ newBallot : ballots
      else do
        putStrLn "Invalid ballot. Try again."
        getBallotOrQuit candidates ballots

validateBallot :: [Char] -> Ballot -> Bool
validateBallot eligibles ballot
  | length (nub ballot) /= length ballot = False
  | any (`notElem` eligibles) ballot = False
  | otherwise = True

printCounts :: CandidateInfo -> [Count] -> IO ()
printCounts = printCountsIndexed 1

printCountsIndexed :: Int -> CandidateInfo -> [Count] -> IO ()
printCountsIndexed _ _ [] = return ()
printCountsIndexed countNumber candidates (c:counts) = do
  putStrLn $ "Count " ++ show countNumber ++ " - " ++ "(Total: " ++ show total ++ ", Target: " ++ show ((total `div` 2) + 1) ++ ")" ++ ": "
  printCount candidates c
  when (null counts)
    $ putStrLn $ "\n" ++ (candidates M.! fst (head c)) ++ " has won the election!\n"
  printCountsIndexed (countNumber + 1) candidates counts
  where total = sum . map snd $ c

printCount :: CandidateInfo -> Count -> IO ()
printCount _ [] = return ()
printCount candidates (leader:others) = do
  putStrLn $ "    " ++ candidates M.! fst leader ++ ": " ++ show (snd leader)
  printCount candidates others
