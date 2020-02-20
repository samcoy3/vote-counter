module Main where

import VoteCount.Count
import VoteCount.Input

main :: IO ()
main = do
  putStrLn "\nWelcome to vote-counter."
  putStrLn "First, you should enter the list of candidates that are running in the election."
  candidates <- getCandidates
  putStrLn "Next, please enter the ballots for the election."
  ballots <- getBallots candidates
  putStrLn "********    RESULTS    ********"
  printCounts candidates (countUntilWinner ballots)

