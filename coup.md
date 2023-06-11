A

Coup is a social deduction game which involves almost no exchange of cards. It is 
permissible to lie about what you have - I have played rounds with people who never 
even bothered to check. At the beginning you get two cards, and if you lose both of 
them you're out. The basic actions are:

- INCOME      - Recieve one coin.
- FOREIGN AID - Recieve two coins.
- COUP        - Spend seven coins, choose a player that must discard a card.

The winning player is the last one to be alive. This is a vicious and nasty game, 
rewarding treachery. There are only five distinct types of card, each allowing for 
an alternate action to be done on one's turn to the standard two:

- ASSASSIN   - May spend only three coins to choose a player that must discard a card.
- CAPTAIN    - May steal two coins from another player, and cannot be stolen from.
- AMBASSADOR - May draw two cards, then discard any two, cannot be stolen from.
- DUKE       - May draw three coins.
- CONTESSA   - May block assassination.

Because you're totally good to lie about your cards, there is an additional action one 
may take: if a player believes that another player's last turn involved an action that 
was predicated on a lie, they may accuse them. If an accusation is demonstrated to be 
truthful, the liar must discard a card of their choosing. If the defender is able to show 
that they told the truth (by revealig a card), the accuser must discard a card. And there 
it is! Cardstock is intended for trick taking games, so a lot of this was challenging to 
implement, as I wasn't able to use any of the already-written games as a base. Nevertheless,
the computer AIs seem to enjoy lying. Perhaps next I will teach them arson.

    ;; Coup
    ;;
    ;; http://boardgame.bg/coup%20rules%20pdf.pdf
    
    (game
     (declare 4 'NUMP)
     (setup  
      (create players 'NUMP)
      (create teams (0) (1) (2) (3))
      
      ;; Create the deck source
      (repeat 3 (create deck (game iloc STOCK) (deck 
       (INFLUENCE (AMBASSADOR, ASSASSIN, CAPTAIN, CONTESSA, DUKE))))))
     
     ;; Assign points for cards, remove 5 cards from game
     (do 
         (
          (put points 'BASIC
       (((INFLUENCE (AMBASSADOR)) 1)
       (( INFLUENCE   (CONTESSA)) 1)
       (( INFLUENCE       (DUKE)) 1)
       (( INFLUENCE   (ASSASSIN)) 1)
       (( INFLUENCE    (CAPTAIN)) 1)))
    
      (put points 'STEALABLE
       (((INFLUENCE (AMBASSADOR)) 0)
       (( INFLUENCE   (CONTESSA)) 1)
       (( INFLUENCE       (DUKE)) 1)
       (( INFLUENCE   (ASSASSIN)) 1)
       (( INFLUENCE    (CAPTAIN)) 0)))
    
      (put points 'ASSASSINATABLE
       (((INFLUENCE (AMBASSADOR)) 1)
       (( INFLUENCE   (CONTESSA)) 0)
       (( INFLUENCE       (DUKE)) 1)
       (( INFLUENCE   (ASSASSIN)) 1)
       (( INFLUENCE    (CAPTAIN)) 1)))
          
          (shuffle (game iloc STOCK))
          
          (all player 'P     
               (do( 
               (set ('P sto FOREIGNAID) 1)      
               (repeat 2
                       (move (top (game iloc STOCK))
                             (top ('P iloc HAND)))))))))
    
     (stage player
     
      ;; The game ends when there is one survivor.
     
      (end (any player 'P 
        (== ('P sto WON) 1)))    
     
       (choice (
    
        ;; Accuse 	
        
        (any player 'P (do (
         ((== ('P sto LIED) 1) 
      	  (any ('P iloc HAND) 'C 
          (move 'C (bottom (game vloc DISCARD)))))
         ((== ('P sto LIED) 0) 
       	  (any ((current player) iloc HAND) 'C 
          (move 'C (bottom (game vloc DISCARD))))))))
    
        ;; Block Foreign Aid
       
        (do (
         ((== (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) DUKE))) 0)
          (set ((current player) sto LIED) 1))
         ((>= (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) DUKE))) 1)
          (set ((current player) sto LIED) 0))
         (all player 'P (do (    
          (set ('P sto FOREIGNAID) 0))))))
    
        ;; Income
      	
        (inc ((current player) sto HUNDREDMILLIONDOLLARS) 1)	
      
        ;; Foreign Aid
        
        (do (  
         ((== ((current player) sto FOREIGNAID) 0)
          (set ((current player) sto LIED) 1))
         ((>= ((current player) sto FOREIGNAID) 1)
          (set ((current player) sto LIED) 0))	 
         (inc ((current player) sto HUNDREDMILLIONDOLLARS) 2)))
      	
        ;; Standard Coup 
      
        (do ( 
         ((> ((current player) sto HUNDREDMILLIONDOLLARS) 6) 
          (do (
           (any player 'P 
            (any ('P iloc HAND) 'C
             (move 'C (bottom (game vloc DISCARD)))))
      	   (dec ((current player) sto HUNDREDMILLIONDOLLARS) 7))))))
    
        ;; Win (if you're the last one left)
         
         ((== (sum         ((current player) iloc HAND)   using 'BASIC) 
              (sum (union (all player 'P ('P iloc HAND))) using 'BASIC)) 
          (do ( 
           (inc ((current player) sto WON) 1))))
    
        ;; Ambassador swap (Draw two cards (four total now in hand) then discard any two)
      		  
        (do (
         ((== (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) AMBASSADOR))) 0)
          (set ((current player) sto LIED) 1))
         ((>= (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) AMBASSADOR))) 1)
          (set ((current player) sto LIED) 0))
         ((> (size (game iloc STOCK)) 1) 
          (do (
          (repeat 2
           (move (top (game iloc STOCK))
            (top ((current player) iloc HAND))))
          (any ((current player) iloc HAND) 'C 
           (move 'C (top (game vloc DISCARD))))
          (any ((current player) iloc HAND) 'C 
           (move 'C (top (game vloc DISCARD)))))))))
     
        ;; Duke tax (Take $300,000,000)
            
        (do (
         ((== (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) DUKE))) 0)
          (set ((current player) sto LIED) 1))
         ((>= (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) DUKE))) 1)
           (set ((current player) sto LIED) 0))
         (inc ((current player) sto HUNDREDMILLIONDOLLARS) 3)))
        
        ;; "Americans are so spoiled. They think you always have to have a car, whereas I got away on my own two feet."
    
        (do (
         ((== (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) ASSASSIN))) 0)
          (set ((current player) sto LIED) 1))
         ((>= (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) ASSASSIN))) 1)
          (set ((current player) sto LIED) 0))	
         ((> ((current player) sto HUNDREDMILLIONDOLLARS) 2)
          (do (   
           (dec ((current player) sto HUNDREDMILLIONDOLLARS) 3)
           (any player 'P 
            ((< (sum ('P iloc HAND) using 'ASSASSINATABLE) 1)
         	   (any ('P iloc HAND) 'C  
              (move 'C (top (game vloc DISCARD)))))))))))  	
    
        ;; Captain steal
      	
        (do (
         ((== (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) CAPTAIN))) 0)
          (set ((current player) sto LIED) 1))
         ((>= (size (filter ((current player) iloc HAND) 'H (== (cardatt INFLUENCE 'H) CAPTAIN))) 1)
          (set ((current player) sto LIED) 0))	
         (any player 'P (
          (and (>= ('P sto HUNDREDMILLIONDOLLARS) 2) (< (sum ('P iloc HAND) using 'STEALABLE) 1))
          (do (   
            (inc ((current player) sto HUNDREDMILLIONDOLLARS) 2)
      	  (dec ('P sto HUNDREDMILLIONDOLLARS) 2))))))))))
        
     
     ;; Player with the smallest hand size wins
     (scoring max ((current player) sto WON)))
    
