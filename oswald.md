A

Haha, yes! I invented this one, or rather, am in the process of inventing this one. Quick little 
abstract. Ludii's meant for abstracts, figured implementing one would help me understand the framework. 
Each player gets 18 stones of their color, and we play on a 8x8 square grid, laying one per 
turn. Stones can be removed and summoned back to one's stock in two ways: 

1. With one's turn, they may recall two stones of their own, attacker's choice, and one of the opponents', also attacker's choice. Cannot be done if you've >2 stones.

2. If one creates an orthogonal representation of one of the 12 free pentominoes, the player that did it gets x points, and that pentomino is no longer capable of scoring. How do we calculate x? To be obtuse about it, x is eight (because that's the order of group D_8) divided by the amount of rotational and reflectional symmetries of a given pentomino. This is a natural way of representing the difference in difficulty of creating the various forms that cannot tie. Here's an enum:


	
                     |  O   
                     | OOO      - 8 POINTS (ONLY ONE)     
                     |  O     
	                 |
                     |         O      
                     |         O            
                     |  OOOOO  O  - 4 POINTS (TWO WAYS)         
                     |         O      
                     |         O       
      	             |    
                     |                                                     OO  OO   O      O      
                     |  ...       - 2 POINT                           (EX. O    O   OOO  OOO ) (FOUR WAYS)       
                     |                                                    OO    OO    O  O           
      	             | 		      
                     |                                      O    O   O     OO  OO     O   O    O     
                     |  ...       - 1 POINT           (EX. OOO   OO  OOO  OO    OO  OOO  OO   OOO ) (ETC.)         
                     |                                       O  OO    O    O    O    O    OO  O     
								 
You win if you get 12 points. I'm eager to fiddle with the scoring (1,2,3,5 in particular seems like 
it would change a lot) and see what results in the most interesting behavior. I also might try
incorporating piece movement. Basically just tetris and gomoku. I'm curious as to what this will look like as a series of ludemes.
