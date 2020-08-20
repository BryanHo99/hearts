-- | Write a report describing your design and strategy here.
{--
    I tried to implement a Heuristic player where the player tries to make different decisions based on certain conditions that occur. 
    All the past tricks are stored in the memory. The memory is mainly used to check if Hearts are broken or the Queen of Spades (SQ) has
    already been played to allow the player to make different decisions to make sure that hopefully they do not gain points in the current trick.
    The player will try to discard their SQ in their hand safely whenever possible based on their memory.
    
    When the leading suit is Hearts, the player tries to play their smallest card so that they do not earn points. If the leading suit is Spades,
    the player tries to play a card that is the second highest card in the trick so that they do not risk winning a trick with an SQ in it.
    If it is a non-point suit that is led, the player will try to discard their highest card possible so that they can lead with their
    smallest card on the next round with the condition if no point cards are currently played in the trick.

    When the player is leading, they will prioritise leading with the smallest non-point card first. If not possible, they will play the smallest
    Spade card available that is not SQ. If not possible either, then they are forced to play SQ if Hearts is not broken. If Hearts is broken 
    however, they will then prioritise leading the smallest Hearts card first.
--}

module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types

playCard :: PlayFunc
playCard _ _ [] Nothing        = (Card Club Two, "")                                            -- First round but player has to lead, so play C2
playCard _ hand _ Nothing      = (firstRenege (haveSuit Club (sort hand)) (sort hand), "")      -- First round and player must not renege
playCard _ hand [] memory                                                                       -- Not the first round but player has to lead
    | heartsPlayed [] (showMemory memory)   = (leadHearts . leadNoSQ . leadNonPoint . sort $ hand, showMemory memory)   -- Hearts is broken
    | otherwise                             = (breaking . leadNonPoint . sort $ hand, showMemory memory)                -- Hearts is not broken
-- Not the first round and player must not renege
playCard _ hand trick memory    = (reneging leader (sort hand) (sort . cardsInTrick $ trick) (showMemory memory), showMemory memory)
    where leader                = (suit $ fst $ last trick)

-- | The bleeding function returns the cards that do not cause Bleeding.
bleeding :: [Card] -> [Card]
bleeding hand = filter (\x -> suit x /= Heart && x /= (Card Spade Queen)) hand

-- | The breaking function returns the smallest card that do not cause Breaking if possible.
breaking :: [Card] -> Card
breaking hand = case filter (\x -> suit x /= Heart) hand of
    [] -> head hand
    xs -> head xs

-- | The leadHearts function returns the smallest Hearts card if possible.
leadHearts :: [Card] -> Card
leadHearts hand = case filter (\x -> suit x == Heart) hand of
    [] -> head hand
    xs -> head xs

-- | The leadNoSQ function returns a list of cards that does not have SQ if possible.
leadNoSQ :: [Card] -> [Card]
leadNoSQ hand = case filter (\x -> x /= (Card Spade Queen)) hand of
    [] -> hand
    xs -> xs

-- | The leadNonPoint function returns a list of cards that are Diamonds or Clubs only if possible.
leadNonPoint :: [Card] -> [Card]
leadNonPoint hand = case filter (\x -> suit x == Diamond || suit x == Club) hand of
    [] -> hand
    xs -> xs

-- | The haveSuit function returns a list of cards from the player's hand that contains the suit that matches the leader.
haveSuit :: Suit -> [Card] -> [Card]
haveSuit leader hand = filter (\x -> suit x == leader) hand

-- | The maxCard function returns the highest card of the leading suit that the player has.
maxCard :: Suit -> [Card] -> Card
maxCard leader hand = last $ filter (\x -> suit x == leader) hand

-- | The showMemory function returns a string of all the cards played in the past and the cards played in the current trick.
showMemory :: Maybe ([(Card, PlayerId)], String) -> String
showMemory Nothing                = ""
showMemory (Just (trick, memory)) = (show . cardsInTrick $ trick) ++ "," ++ memory

-- | The spadeQueenPlayed function checks if SQ already played in the previous rounds or it is played in the current trick.
spadeQueenPlayed :: [Card] -> String -> Bool
spadeQueenPlayed trick memory = "SQ" `isInfixOf` memory || (Card Spade Queen) `elem` trick

-- | The heartsPlayed function checks if Hearts has been broken in previous rounds or there is a Heart in the current trick.
heartsPlayed :: [Card] -> String -> Bool
heartsPlayed trick memory = "H" `isInfixOf` memory || find (\x -> suit x == Heart) trick /= Nothing

-- | The bestSpadeCard function plays the highest card in hand that is lesser than SQ.
bestSpadeCard :: [Card] -> Card
bestSpadeCard hand = case filter (\x -> suit x == Spade && rank x < Queen) hand of
    [] -> last hand
    xs -> last xs

-- | The select function returns the specific card if the player's hand has it, or else it returns the highest card in their hand.
select :: Maybe Card -> [Card] -> Card
select Nothing hand       = last hand                 -- Discard any highest card
select card _             = fromJust card             -- Discard specific card

-- | The cardsInTrick function returns only the cards of the current trick.
cardsInTrick :: [(Card, PlayerId)] -> [Card]
cardsInTrick trick = (\x -> fst x) <$> trick

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

-- | Given a card, select its rank.
rank :: Card -> Rank
rank (Card _ r) = r

-- | The firstRenege function handles the first round where players must not bleed. The player tries to play the highest Clubs card so that
-- | they can lead on the next round.
firstRenege :: [Card] -> [Card] -> Card
firstRenege [] hand       = case bleeding hand of
    [] -> select (find (== (Card Spade Queen)) hand) hand  -- If only have Hearts and SQ, play SQ. Otherwise, play highest Hearts card (very rare)
    xs -> last xs                                          -- If no Clubs, play the highest card that will not cause bleeding
firstRenege xs _          = last xs                        -- If have Clubs, play highest Clubs card (to lead next round)

-- | The reneging function takes in a leader suit, finds all the cards of that suit from the player's hand, and selects the best one
-- | from the bunch by comparing them with the ranks of the current trick.
reneging :: Suit -> [Card] -> [Card] -> String -> Card
reneging Heart hand _ _                  = renegeHearts (haveSuit Heart hand) hand                            -- Renege Hearts
reneging Spade hand trick memory                                                                              -- Renege Spades
    = renegeSpades (spadeQueenPlayed trick memory) (haveSuit Spade hand) hand (rank $ maxCard Spade trick)
reneging nonPointSuit hand trick memory                                                                       -- Renege Diamonds/Clubs
    = renegeNonPointCards pointCardsPlayed (haveSuit nonPointSuit hand) hand (rank $ maxCard nonPointSuit trick)
    where
        pointCardsPlayed        = (spadeQueenPlayed trick memory) || (heartsPlayed trick memory)      

-- | The renegeHearts function causes the player to make different decisions depending on the conditions given when the leading suit is Hearts.
renegeHearts :: [Card] -> [Card] -> Card
-- Player does not have Hearts, try to play SQ if they have
renegeHearts [] hand             = select (find (== (Card Spade Queen)) hand) hand
-- Player has Hearts card, play the smallest Hearts card
renegeHearts xs _                = head xs

-- | The renegeSpades function causes the player to make different decisions depending on the conditions given when the leading suit is Spades.
renegeSpades :: Bool -> [Card] -> [Card] -> Rank -> Card
-- Player does not have Spades, so try to play highest Hearts card (break Hearts) or else just discard the highest card in hand
renegeSpades _ [] hand _                       = check (haveSuit Heart hand) where
    check []            = last hand
    check xs            = last xs
-- Player has Spades and SQ played, so play best card in hand
renegeSpades True xs hand highestCardInTrick   = bestCard xs hand highestCardInTrick
-- Player has Spades but SQ is not played, so try to play the highest Spade Card that is smaller than SQ
renegeSpades False xs _ _                      = bestSpadeCard xs

-- | The renegeNonPointCards function causes the player to make different decisions depending on the conditions given when the leading suit is
-- | a Diamond or a Club.
renegeNonPointCards :: Bool -> [Card] -> [Card] -> Rank -> Card
-- Player cannot follow suit, so try to play SQ
renegeNonPointCards _ [] hand _                             = select (find (== (Card Spade Queen)) hand) hand
-- Player can follow suit and point cards are in the trick, so play best card in hand
renegeNonPointCards True xs hand highestCardInTrick         = bestCard xs hand highestCardInTrick
-- Player can follow suit but point cards are not in the trick, so play highest card
renegeNonPointCards False xs _ _                            = last xs

-- | The bestCard function tries to find the highest card the player has that is lesser than the current highest card in the trick.
bestCard :: [Card] -> [Card] -> Rank -> Card
-- Player does not have a card that follows suit, discard highest card
bestCard [] hand _                  = last hand
bestCard xs _ highestCardInTrick    = case filter (\x -> rank x < highestCardInTrick) xs of
    [] -> head xs                  -- Player discards smallest card available if no card is smaller than highest card in trick
    ys -> last ys                  -- Player has a card that is lesser than the highest card in the trick

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined