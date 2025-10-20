module CourseworkOne where

import Halatro.Constants
import Halatro.Types
import Data.List
import Data.Set 
import Data.Ord

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

contains :: Hand -> HandType -> Bool
contains hand handType = 
  case handType of
    None          -> hand == []  -- If the hand is empty then handType = None
    HighCard      -> (hand /= []) && (not $ hasNTuple hand 2 1)  -- If the hand does not contain a pair and is not empty, then it's a high card.
    Pair          -> hasNTuple hand 2 1
    TwoPair       -> hasNTuple hand 2 2
    ThreeOfAKind  -> hasNTuple hand 3 1
    Straight      -> isStraight hand
    Flush         -> isFlush hand
    FullHouse     -> isFullHouse hand
    FourOfAKind   -> hasNTuple hand 4 1
    StraightFlush -> isStraightFlush hand
    RoyalFlush    -> isRoyaleFlush hand

-- These functions are basic but make my code more self documenting

handListToSet :: Hand -> Set Card 
handListToSet = fromList 

getHandRanks :: Hand -> [Rank]
getHandRanks = Data.List.map rank

getHandSuits :: Hand -> [Suit]
getHandSuits = Data.List.map suit

getSetRanks :: Hand -> Set Rank
getSetRanks = fromList . getHandRanks

getSetSuits :: Hand -> Set Suit 
getSetSuits = fromList . getHandSuits

--
-- This section of code handles the calculation of hands which involve pairs

-- This function takes a Hand and returns an integer list 
-- Each integer in the list represents the number of times a unique rank appeared 
occuranceOfRanks :: Hand -> [Int]
occuranceOfRanks hand = 
    let 
        -- Get unique ranks from the hand
        uniqueRanks = getSetRanks hand 
        
        -- Takes a rank and the current accumulator
        -- Prepend the number of times that rank appears in the hand to the accumilator
        addCount :: Rank -> [Int] -> [Int]
        addCount r acc = countOccurrenceOfRank r : acc

        -- Takes a rank and returns the number of times that rank appears in the hand
        countOccurrenceOfRank :: Rank -> Int
        countOccurrenceOfRank r = length $ Data.List.filter (\card -> rank card == r) hand

        -- Counts uses foldr to iterate over a list of unique ranks to produce a list which contains the occurance of each rank
        counts = Data.Set.foldr addCount [] uniqueRanks
    in 
        counts

-- This function takes a hand, a required tuple (pair = 2, three of a kind = 3, four of a kind = 4)
-- and takes the minimum count that this tuple appears (pair = 1, two pair = 2, three of a kind = 1, four of a kind = 1)
-- Then applies these values to the result of the occurance of rank method
hasNTuple :: Hand -> Int -> Int -> Bool
hasNTuple hand requiredTuple minCount = (length $ Data.List.filter (>=requiredTuple) (occuranceOfRanks hand)) >= minCount

isFullHouse :: Hand -> Bool
isFullHouse hand = 
    let counts = sort $ occuranceOfRanks hand 
    in counts == [2,3]
--
-- This section of code handles the calculation of hands which do not involve pairs

isFlush :: Hand -> Bool 
isFlush hand 
    | length hand /= 5 = False
    | otherwise = Data.Set.size (getSetSuits hand) == 1

isStraight :: Hand -> Bool 
isStraight hand 
    | length hand /= 5 = False
    | otherwise = and consecutive || isAceLow 
     where 
        -- Sort the hand by rank
        sortedRanks = sort $ getHandRanks hand 

        -- Take 2 lists (sortedRanks and the tail of sortedRanks)
        -- For each element of each list, compare the difference in rank
        -- If the difference is 1, then true otherwise false
        -- Return a list of bools for each comparison
        consecutive = zipWith (\a b -> fromEnum b - fromEnum a == 1) sortedRanks (tail sortedRanks)

        -- Handles the edge case where the ace is the lowest card in the straight
        isAceLow = sortedRanks == [Two, Three, Four, Five, Ace]

isStraightFlush :: Hand -> Bool 
isStraightFlush hand = isStraight hand && isFlush hand

isRoyaleFlush :: Hand -> Bool
isRoyaleFlush hand =
    isFlush hand && (sort (getHandRanks hand) == [Ten, Jack, Queen, King, Ace])

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

-- This function makes a list containing all the playable hand types
-- It does this by passing every hand type and filtering the ones out which are not true for contains hand
-- Then it returns the maximum of this filtered list
bestHandType :: Hand -> HandType
bestHandType hand = maximum $ Data.List.filter (contains hand) [None .. RoyalFlush]

--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- This method takes a hand and returns the cards in the hand which contribute to the hand scoring
-- It does this by patternmatching on bestHandType hand, which will determine the type of hand played
-- It then based on the hand played will calculate which cards in the hand correspond to that hand type
whichCardsScore :: Hand -> [Card]
whichCardsScore hand =
    case bestHandType hand of 
        None            -> []
        HighCard        -> [maximumBy (comparing rank) hand] 
        Pair            -> getScoringPairs hand
        TwoPair         -> getScoringPairs hand
        ThreeOfAKind    -> getScoring3OfKind hand
        Straight        -> hand
        Flush           -> hand
        FullHouse       -> hand
        FourOfAKind     -> getScoring4OfKind hand 
        StraightFlush   -> hand
        RoyalFlush      -> hand

-- Divides the hand into sublists of cards of equal rank
groupByRank :: Hand -> [[Card]]
groupByRank = groupBy (\a b -> rank a == rank b) . sortBy (comparing rank)

-- Gets and concats all sublists of length 2 from groupByRank
getScoringPairs :: Hand -> [Card]
getScoringPairs = concat . Data.List.filter (\grp -> length grp == 2) . groupByRank

-- Gets and concats all sublists of length 3 from groupByRank
getScoring3OfKind :: Hand -> [Card]
getScoring3OfKind = concat . Data.List.filter (\grp -> length grp == 3) . groupByRank

-- Gets and concats all sublists of length 4 from groupByRank
getScoring4OfKind :: Hand -> [Card]
getScoring4OfKind = concat . Data.List.filter (\grp -> length grp == 4) . groupByRank

scoreHand :: Hand -> Int
scoreHand hand = 
    let 
        -- Pass the best hand type of a given hand into the handTypeValues function 
        (baseChips, multi) = handTypeValues $ bestHandType hand

        -- Map the rank function composed with the rankScore function over the result of whichCardsScore hand and take the sum
        bonusChips = sum $ Data.List.map (rankScore . rank) (whichCardsScore hand)
    in (baseChips + bonusChips) * multi 

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

-- This takes a list of cards c and an integer n
-- It returns all unqiue lists of length n which are a subset of c 
handCombinations :: Int -> [Card] -> [[Card]]
handCombinations 0 _ = [[]] -- n choose 0 == [[]]
handCombinations _ [] = [] -- No combinations of an empty list => []
handCombinations k (x:xs) = Data.List.map (x:) (handCombinations (k-1) xs) ++ (handCombinations k xs)

-- This funtion takes a list of cards and returns the cards which produce the highest score
-- Note that this function only returns the smallest hand which produces the highest score
-- I want the strictest hand to be returned so when I play a hand I can add cards to 'discard' when playing a hand
highestScoringHand :: [Card] -> Hand
highestScoringHand deck
    | deck == [] = []   --     
    | (length deck) <= 5 = whichCardsScore deck
    | otherwise = whichCardsScore $ maximumBy (comparing scoreHand) (handCombinations 5 deck)

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

simpleAI :: [Move] -> [Card] -> Move
simpleAI _ hand = Move Play (Data.List.take 5 (reverse $ sortBy (comparing rank) hand))

sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ hand = Move Play (highestScoringHand hand)

-- myAI optimises for flushes, for the reason why see here https://youtu.be/1ocfkhv8r_I?si=S7EnbAnNStlRqdHe
-- Optimising for flushes is pretty intuitive, just do everything in your power to build to 5 of a suit

myAI :: [Move] -> [Card] -> Move 
myAI pastMoves hand 
    | playsRemaining == 1 && discardsRemaining > 0  = optimiseDiscardForFlush hand -- If only one play left should exhaust all discards to get the best hand possible
    | currentBestScore >= 160                       = bestHandToPlay hand -- If a good hand then play it (160 seems to be the sweetspot)
    | discardsRemaining > 0                         = optimiseDiscardForFlush hand -- If a bad hand and have discards remaining then don't play and use discards
    | otherwise                                     = bestHandToPlay hand
    where 
        discardsUsed = length $ Data.List.filter (isDiscardMove) pastMoves
        playsUsed = length $ Data.List.filter (isPlayMove) pastMoves
        discardsRemaining = 3 - discardsUsed
        playsRemaining = 3 - playsUsed
        currentBestScore = scoreHand (highestScoringHand hand)

-- Is True if the move is a discard move
isDiscardMove :: Move -> Bool
isDiscardMove move =
    case move of
        Move Discard _ -> True
        _              -> False

-- Is True if the move is a discard move
isPlayMove :: Move -> Bool 
isPlayMove move =
    case move of 
        Move Play _    -> True 
        _              -> False

-- Divides the hand into sublists of cards of equal suit
groupBySuit :: Hand -> [[Card]]
groupBySuit = groupBy (\a b -> suit a == suit b) . sortBy (comparing suit)

-- Calculates the most commmon suit by first finding the largest set of cards which all have the same suit
-- Then takes a card from this set and returns its suit
mostCommonSuit :: Hand -> Suit 
-- To extract a card from the flushSuit list (The list that is returned of all cards which are of the most common suit), I just used the maximumBy function again 
mostCommonSuit hand = suit $ maximumBy (comparing rank) (maximumBy (comparing length) (groupBySuit hand))

-- If I want to optimise for a flush, then everytime I play, I want to play 5 cards
-- And ensure that the cards that don't score aren't of the flushsuit
-- Filler are the cards which I want to play but don't score
bestHandToPlay :: Hand -> Move  
bestHandToPlay hand 
    | spaceInPlay <= 0  = Move Play bestCards
    | otherwise         = Move Play (bestCards ++ filler)
    where 
        bestCards = highestScoringHand hand 
        spaceInPlay = 5 - length bestCards
        remainingCards = hand Data.List.\\ bestCards 
        flushSuit = mostCommonSuit hand 
        nonFlushCandidates = Data.List.filter (\card -> suit card /= flushSuit) remainingCards
        sortedNonFlushCandidates = sortBy (comparing rank) nonFlushCandidates
        filler = Data.List.take spaceInPlay (sortedNonFlushCandidates)

-- If I want to optimise for a flush, then everytime I discard, I want to discard as many cards as possible
-- And ensure that the cards that don't score aren't of the flushsuit
optimiseDiscardForFlush :: Hand -> Move 
optimiseDiscardForFlush hand =  Move Discard (Data.List.take 5 sortedNonFlushCards) -- Take 5 just ensures I always discard five cards if I can
    where 
        flushSuit = mostCommonSuit hand 
        nonFlushCards = Data.List.filter (\card -> suit card /= flushSuit) hand 
        sortedNonFlushCards = sortBy (comparing rank) nonFlushCards
        numberOfFlushCards = length $ Data.List.filter (\card -> suit card == flushSuit) hand 
