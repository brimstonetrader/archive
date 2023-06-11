Most Ludii games are abstract strategy, played by rearranging nodes on some sort of graph. 
The easiest Ludeme to understand in this context is the Line. The code snippet

    (is Line 3)
  
is a boolean that is true if any three of the moving player's stones/pieces/nodes/units are 
arranged in a 3-long line, and it is true on a square, triangular, hexagonal, 3.4.3.3.4, or 
arbitrary user-specified graph.

There exists a generalization of this ludeme called the "Pattern". They are created by 
making a list of some arbitrary combination of three basic movements: Forward (F), Right, 
(R), and (L). Thus, 

    (is Pattern {F F}) = (is Line 3)

The most common usage is to detect the formation of a square, which looks like this.

    (is Pattern {F R F R F})
  
See, you start at some arbitrary spot, go forward one (now you've been to two blocks total), 
turn right then go forward one, (three now), then turn right for the second time, now facing
down, and inch forward one to complete the formation. If I switched the tiling, without changing 
that line, the new win formations would be:

    |TRIANGULAR       SQUARE        HEXAGONAL
  
    |    O              OO             OO
  
    |   O O             OO            O  O
  
I first got really into abstract strategy games through Wikipedia. I've read many, many articles 
on the topic, and as such I am confident when I say that Teeko has the funniest one. It's a very 
simple game, one of the ones I'll play with small children or adults who don't like to think. You
lay eight nodes on a 5x5 square grid, then move them like Chess kings until you've got a 4-line or a 
square. The game's inventor was exceedingly proud of his creation, with the wikipedia article stating 

 - The rules, as summarized above, are very simple, but the strategy is complicated enough 
   to fill a book, <ins>Scarne on Teeko</ins>, by Scarne (1955).
   
That's not what's funny though. Some guy, literally named Guy, comes along in 1998 (wikipedia 
immediately goes into this after the above sentence) and solves Teeko with a computer. Ok, fine.
They had Chess solved by then. But Teeko was more than just a game to Scarne. He described 16 variants
in one of his books on the game: it was many games. Within that year, Guy solved all 16, and publicized
his results. Hilarious! 
   
Anyway, uh. Teeko was one of the medium difficulty Ludii wishlist games, so here it is:

	(game "Teeko" 
		(players 2) 
		(equipment { 
			(board (square 5)) 
			(hand Each size:8)
			(piece "Marker" Each ("StepToEmpty"))
		}) 
		(rules 
			(start { 
				(place "Marker" "Hand" count:8)
			})
			
			phases:{
			(phase "Placement"
				(play (move ("FromHand") (to (sites Empty))))
				(nextPhase ("HandEmpty" P2) "Movement")
			)
			
			("PhaseMovePiece" "Movement")
			}
			
			(end 
				(if 
					(or {
						(is Pattern {F R F R F R F})
						(is Line 4)
					})
					(result Mover Win)
				)
			) 
		)
	)
  
I also implemented Christian Freeling's "Hexade," which is similar to Pente on a hexagona board, with custodial capture and a win on a line of 6, and the additional win conditions of the formation of a triangle or punctured hexagon. 

  	(game "Hexade" 
		(players 2) 
		(equipment { 
			(board (tri 8)) 
			(piece "Marker" Each) 
		}) 
		(rules 
			(meta (swap))    
			(play (move Add (to (sites Empty)) (then "Custodial")))
			(end 
				(if 
					(or {
						(is Pattern {F R F R F R F R F})
						(is Line 6)
						(is Pattern {F F R R F F R R F})
						(is Pattern {F R R F F R R F F})
					})
					(result Mover Win)
				)
			) 
		)
	)

The triangle requires two checks: one for if the final node is placed in the center, one if it is 
placed on the edge. Because I know you're just oozing for it, here is a breakdown of what these look
like on the three regular tilings:


    |TRIANGULAR       SQUARE        HEXAGONAL
  
    |           {F R F R F R F R F}             
 
    |    OO             OO             OO
  
    |   O  O            OO            O  O
    
    |    OO                            OO
    
    |               {F F F F F}             
 
    |                                
  
    |   OOOOOO         OOOOOO          OOOOOO
    
    |                                
   
    |          {F R R F F R R F F}             
 
    |   OO              O               O
  
    |   O OO            O               OO
    
    |   O               O              OOO
    
I'm not quite doing justice how cool that last triangular one looks. It's like a ninja star. 
    
    
    
