A 

So this summer Mason, Dr. Goadrich, and I are doing an Odyssey project together where we're analyzing card games, gathering statistics with a supermassive AI array that just sits around playing all day. The challenge, this first week, has been for Mason and I to understand the syntax by which we communicate the rules of card games to the AIs. To demonstrate this understanding, obviously, the two of us wrote some card games. The first one I wrote was

## POKER.

This was the first game I asked about, because to a very passive observer, it appears to be the most popular game involving cards. The Lisp-ish syntax, ReCYCLE, was built for trick-taking games in particular. Dr. Goadrich said that it was technically possible, but that thus far no one had felt the calling to brave the "combinatoric explosion" involved in scoring such a thing with the scant five basic integer operations included in CardDECK (+,-,*,%,//). It wasn't too bad, to tell you the truth. Let me show you how it's written. 


    (game
     (setup  
      (create players 5)
      (create teams (0) (1) (2) (3) (4))
      (create deck (game iloc DECK) (deck (RANK  (A, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, J, Q, K))
                                           (COLOR (RED (SUIT (HEARTS, DIAMONDS)))
                                                (BLACK (SUIT (CLUBS,  SPADES)))))))     

The easiest thing to notice about ReCYCLE's basic structure is that there are a preponderance of parentheses. This is for type-checking reasons, which I will spend no further time justifying. The setup block for the majority of cardDECK games look indistinguishable from one another - all that happens here is the creation of the deck of cards and establishment of the number of players. Teams, while not used here, come into play in games like Spades, where that line would look like 

    (create teams (0, 2) (1, 3))

. Currently, the AIs can't quite fathom collaborative games, like (0, 1, 2, 3), but this syntax does allow for them. The next interesting part is 
 
      ;; bidding.
      (do (
        (shuffle (game iloc DECK))
        (put points 'SCORE (
                ((RANK (A))    12)
                ((RANK (K))    11) 
                ((RANK (Q))    10)
                ((RANK (J))     9)
                ((RANK (TEN))   8)
                ((RANK (NINE))  7)
                ((RANK (EIGHT)) 6)
                ((RANK (SEVEN)) 5)
                ((RANK (SIX))   4)
                ((RANK (FIVE))  3)
                ((RANK (FOUR))  2)
                ((RANK (THREE)) 1)
                ((RANK (TWO))   0)))
        (all player 'P (any (range 1..10) 'B
         (do (
          (set ('P sto CHIPS) 100)
          (set ('P sto BET) 'B)))))))
	
ReCYCLE needs a preceding "(do (" when you want to do something, or several things. Here, we do several things, shuffling the deck, letting each player have a hundred chips, and bet between one and ten of them on the round to come. That point-map is pretty basic, but it becomes important when we get to the scoring step. The next step, in this very vanilla version of poker, is for all
 
      ;; players to get their first five cards.
      
      (do ((repeat 5 (move (top (game iloc DECK)) 
       (top ((current player) iloc HAND))))))
 
ReCYCLE runs on ilocs and vlocs - invisible and visible locations. Cards being moved from location to location is the main thing that happens in any given game, and if a card is visible, at any point, the AIs take note. In this movement, from the deck to the hand, all cards are invisible, or face-down, throughout.

      (choice (
        
        ;; Discard 0
        
        (turn pass)
         
        ;; Discard 1
        (do (
         (any ((current player) iloc HAND) 'A 
             (move 'A (top (game vloc DISCARD))))))
           
        ;; Discard 2
         
        (do (
         (any ((current player) iloc HAND) 'C 
          (move 'C (top (game vloc DISCARD))))
         (any ((current player) iloc HAND) 'CC 
          (move 'CC (top (game vloc DISCARD))))
         (set ((current player) sto FINISHED) 1)))
       
        ;; etc.
       
        (do (
         (any ((current player) iloc HAND) 'D 
          (move 'D (top (game vloc DISCARD))))
         (any ((current player) iloc HAND) 'DD 
          (move 'DD (top (game vloc DISCARD))))
         (any ((current player) iloc HAND) 'DDD 
          (move 'DDD (top (game vloc DISCARD))))
         (set ((current player) sto FINISHED) 1)))))

Next, each player has a choice between four actions, discarding somewhere between zero and three cards. Our AI is based on Monte-Carlo Tree Search, so it will consider many random possible games that could follow from each option, and choose the one where they generally stand the best chance of winning.

      (choice (
       (any (range 1..10) 'BB
        (do (
         (set ((current player) sto BETT) 'BB)
         (inc ((current player) sto BET) ((current player) sto BETT))
                   
         (repeat (- (size ((current player) iloc HAND)) 5) 
             (move (top (game iloc STOCK)) 
              (top ((current player) iloc HAND)))))))))

Next, as their last conscious action, the players may again bet between 1 and 10 of their chips. This amount is added to the bet previous, through that line beginning with "inc", for "increment." Both BET and BETT are integer storage locations ancillarily linked to each player, independent of their hand of cards.

# SCORING

The most difficult aspect of writing poker in ReCYCLE was scoring the hands. The first thing I did was some pseudocode, determining a general enumeration of poker hands that seemed doable in ReCYCLE, which is as follows.

      0-12  -> Junk hands ranked by highest card (impossible to get 0-4, but makes code cleaner)
       find highest card in hand
       set score = maxinhand[card[value]] 
     
     13-25  -> One Pair by what it is (ties are allowed)
       if for any rank S the sum of cards in the hand of rank S is 2 then the score is (13+value[thecardthatthepairis]) 
     
     26-181 -> Two Pair by first then next (many unnecessary scores to make calculations easier)
       if (score > 12) 
       then if for any rank S 
                the sum of cards in the hand of rank S with value (score-13) is 2 
            and the sum of cards in the hand of rank S with value j is 2 
            then newscore = (if (j>(score-13) then (j*13 + (score-13) + 26))
                        else if (j<(score-13) then (13*(score-13) + j + 26) 
                           else score             
   
    182-194 -> Three of a Kind
       if for any rank S
           the sum of cards in the hand of rank S with value j is 3 
            score = 182+j
   
    195-207 -> Straight (once again, 197 196, 195, and 194 are impossible)
       if for any rank S and value j
           the sum of cards in the hand of rank S with value j is 1
       and the sum of cards in the hand of value j+1 is 1
       and the sum of cards in the hand of value j+2 is 1
       and the sum of cards in the hand of value j+3 is 1 
       and the sum of cards in the hand of value j+4 is 1
        score=195+j+4
   
    208-220 -> Same Suit by highest card
       if for any suit S
           the sum of cards in the hand of suit S is 5 
            score = 208+max[cardinhand[value]] 
    218-230 -> Full House (a tie on the three is impossible)
       if (181<score<195) then if for any rank S with value j
                                   the sum of cards in the hand of rank S is 2
                             then newscore = 218+(oldscore-181)
    231-243 -> Four-of-a-Kind
       if for any rank S
           the sum of cards in the hand of rank S with value j is 4
            score = 231+j
    244-253 -> Suit Run
       if (for any rank S and value j
            the sum of cards in the hand of rank S with value j is 1
        and the sum of cards in the hand of             value j+1 is 1
        and the sum of cards in the hand of             value j+2 is 1
        and the sum of cards in the hand of             value j+3 is 1 
        and the sum of cards in the hand of             value j+4 is 1) and 
       if (for any suit S
           the sum of cards in the hand of suit S is 5 
            score = 244+j)    

1. 0-12  -> Junk hands ranked by highest card

Technically, if you've got pure junk, the lowest possible hand would look like 23457, which has a value of 6 here. However, since all we care about is ranking hands, it's not super important that we account for that. Thus, here, and in a few other spots, there will be "impossible scores," which allow for each type of hand to increment by a multiple of 13, letting a lot of minute calculations be doable with a limited toolbelt, while still ranking hands correctly. To rank the hands according this, we 

      (do (
    
       ;; score = highcard
      
       (set ((current player) sto POINTS) (score (max ((current player) iloc HAND) using 'SCORE) using 'SCORE))

. Next, we must do single pairs. In ReCYCLE, pairs of the same rank can tie: the high card outside of the pair is not factored in. In the future, if I implement poker in Ludii, I will attempt to account for this, but at the moment the corresponding numeric scores to one-pairs are

2.  13-25  -> One Pair by what it is 

       ;; duplicate
    
       (any ((current player) iloc HAND) 'P 
        ((== 
         (size (filter ((current player) iloc HAND) 'PP 
         (== (cardatt RANK 'PP) (cardatt RANK 'P)))) 2)
          (set ((current player) sto POINTS) (+ (score 'P using 'SCORE) 13))))

In code, we loop over every card in a given player's hand, then see if any of them have the same rank as precisely one other card in the had. This code will not consider three or four of the same card to be a pair, which is good. Addition, and all integer operations here, use prefix notation, so that final "set score" line is just adding 13 to the rank of the pair, according to the 'SCORE pointmap. Variables can be "set" over and over in ReCYCLE, like python, which is useful for incrementing the score without doing a lot more math.

3.  182-194 -> Two Pair by max then min

The two pairs, first we check that the current score 
(I will finish this post later)




  ;; dual duplicate

    ((and (< ((current player) sto POINTS) 26) (> ((current player) sto POINTS) 12))
     (any ((current player) iloc HAND) 'W  
      ((== (size (filter ((current player) iloc HAND) 'WW    
       (== (cardatt RANK 'WW) (cardatt RANK 'W))))    2)
      
      (do (
       ((> (score 'W using 'SCORE) (- ((current player) sto POINTS) 13))
        (set ((current player) sto POINTS)  (+ (* (score 'W using 'SCORE) 13) 
                                                  (+ (- ((current player) sto POINTS) 13) 13))))                                               
       ((< (score 'W using 'SCORE) (- ((current player) sto POINTS) 13))
        (set ((current player) sto POINTS) (+ (* (- ((current player) sto POINTS) 13) 13) 
                                                 (+ (score 'W using 'SCORE) 13)))))))))
             
      ;; triplicate
   
      (any ((current player) iloc HAND) 'R 
       ((== 
        (size (filter ((current player) iloc HAND) 'RR 
        (== (cardatt RANK 'RR) (cardatt RANK 'R)))) 3)
         (set ((current player) sto POINTS) (+ (score 'R using 'SCORE) 182))))
   
      ;; straight
   
      (any ((current player) iloc HAND) 'T 
       (any ((current player) iloc HAND) 'TT 
       ((== (score 'TT using 'SCORE) (+ (score 'T using 'SCORE) 1)) 
        (any ((current player) iloc HAND) 'TTT 
        ((== (score 'TTT using 'SCORE) (+ (score 'T using 'SCORE) 2)) 
         (any ((current player) iloc HAND) 'TTTT 
         ((== (score 'TTTT using 'SCORE) (+ (score 'T using 'SCORE) 3)) 
          (any ((current player) iloc HAND) 'TTTTT 
          ((or (== (score 'TTTTT using 'SCORE) (+ (score 'T using 'SCORE)  4))
              (== (score 'TTTTT using 'SCORE) (+ (score 'T using 'SCORE) 12))) 
            (set ((current player) sto POINTS) (+ (score 'T using 'SCORE) 199)))))))))))
   
   
      ;; same suit
   
       ((or (or (or (== (sum ((current player) iloc HAND) using 'CLUBS)    5) 
                    (== (sum ((current player) iloc HAND) using 'DIAMONDS) 5)) 
                    (== (sum ((current player) iloc HAND) using 'LEARTS)   5)) 
                    (== (sum ((current player) iloc HAND) using 'SPADES)   5))
        (set ((current player) sto POINTS) (+ (score (max ((current player) iloc HAND) using 'SCORE) using 'SCORE) 208)))
        
      ;; full house
   
      (any ((current player) iloc HAND) 'Y 
       ((== (size (filter ((current player) iloc HAND) 'YY (== (cardatt RANK 'YY) (cardatt RANK 'Y)))) 3)
         (any ((current player) iloc HAND) 'X 
           ((== (size (filter ((current player) iloc HAND) 'XX (== (cardatt RANK 'XX) (cardatt RANK 'X)))) 2)
              (set ((current player) sto POINTS) (+ (score 'Y using 'SCORE) 218))))))
   
      ;; quadruplicate      
       
      (any ((current player) iloc HAND) 'U 
       ((== 
        (size (filter ((current player) iloc HAND) 'UU 
        (== (cardatt RANK 'UU) (cardatt RANK 'U)))) 4)
         (set ((current player) sto POINTS) (+ (score 'U using 'SCORE) 231))))
   
      ;; suit run
   
      ((and (< ((current player) sto POINTS) 220) (> ((current player) sto POINTS) 207))
       (any ((current player) iloc HAND) 'T 
        (any ((current player) iloc HAND) 'TT 
         ((== (score 'TT using 'SCORE) (+ (score 'T using 'SCORE) 1)) 
          (any ((current player) iloc HAND) 'TTT 
           ((== (score 'TTT using 'SCORE) (+ (score 'T using 'SCORE) 2)) 
            (any ((current player) iloc HAND) 'TTTT 
             ((== (score 'TTTT using 'SCORE) (+ (score 'T using 'SCORE) 3)) 
              (any ((current player) iloc HAND) 'TTTTT 
               ((or (== (score 'TTTTT using 'SCORE) (+ (score 'T using 'SCORE)  4))
                    (== (score 'TTTTT using 'SCORE) (+ (score 'T using 'SCORE) 12))) 
                 (set ((current player) sto POINTS) (+ (score 'T using 'SCORE) 244))))))))))))))
   
    ;; Find score for players
    (do (
       (all player 'P 
        (repeat all
                        (move (top    ('P iloc HAND))
                              (bottom (game vloc DISCARD))))
        ((>= ((current player) sto POINTS) ('P sto POINTS)) 
        (do (
         (inc ((current player) sto CHIPS) ('P sto BET))
         (dec ('P sto CHIPS) ('P sto BET))
         (inc (game sto ROUNDS) 1)
         (repeat all
                        (move (top    (game vloc DISCARD))
                              (bottom (game iloc STOCK))))
         (set ('P sto BET) 0))))))))
      
    (scoring max ((current player) sto CHIPS)))
