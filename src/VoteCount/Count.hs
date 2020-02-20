module VoteCount.Count where

import qualified Data.Map as M
import Data.List
import Data.Function (on)

type Candidate = Char
type Ballot = [Candidate]
type Count = [(Candidate, Int)]

countOnce :: [Ballot] -> Count
countOnce = sortOn (negate . snd) . M.toList . foldr insertOne M.empty where
  insertOne ballot = M.insertWith (+) (head ballot) 1

countUntilWinner :: [Ballot] -> [Count]
countUntilWinner ballots = if ((*2) . snd . head) countResult <= totalVotes
  then countResult : (countUntilWinner modifiedBallots)
  else [countResult]
  where
    countResult = countOnce ballots
    totalVotes = sum . map snd $ countResult
    modifiedBallots = modifyBallots ballots countResult

modifyBallots :: [Ballot] -> Count -> [Ballot]
modifyBallots ballots count = okBallots ++ redistributedBallots where
  eliminatedCandidates = map fst $ dropWhile (\can -> snd can /= (snd . last $ count)) count
  (ballotsToChange, okBallots) = partition (\b -> head b `elem` eliminatedCandidates) ballots
  redistributedBallots = filter (/= []) . map tail $ ballotsToChange
