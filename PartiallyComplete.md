My strategy for learning Ludii over the first week was effective but left me feeling somewhat 
demotivated. Luckily I cannot be stopped. Ludii maintains a ["wishlist"](https://ludii.games/wishlist.php) where one
can find games that have not been implemented, but that the Ludii community has determined are
good and would be nice to have. I chose three games that I like and have played, greatly underestimating
the difficulty involved in implementing each. Regardless, I learned a lot, and had a good time. Next 
time I'm trying to learn a new thing I won't run up to the "unsolved problems" board and pick out several
like it's the Scholastic Book Fair. Anyway first up is

  - Ayu
  
If you ask me the 20th century really got to going with the simultaneous discovery of Hex by Hein and 
Nash. It was the first game to have strategy based on maintaining connections between groups, a genre of 
abstract strategy games which has been thoroughly explored over the following hundred years. In Hex, you 
want a contiguous group (a bunch of nodes all orthogonally adjacent), that stretches across the entire 
board. This is cool, but it means a game on a standard 11x11 board goes by quick as all get-out, and 
no one wants to play Hex more than 2 or 3 times in a row. 

Lines of Action, which came out in the '80s, has a way cooler win condition, and it's easily playable
on a chessboard with chess pieces. Easily in my top 5 games, just under Minecraft. One's initial nodes 
begin on the board, far apart from each other, and the goal is to unite all of one's pieces into a single 
group. The movement rules, as they have to, make things difficult. In any direction, one can only move a 
node $n$ steps, where $n$ is the total amount of nodes in that entire line. This begs a question: can a 
unification game with standard movement be fun? No one knew until All-Time Legend Luis Bolaños Mures 
popped out a banger called Ayu, which allows for singleton moves and group moves, and does not allow for a
group ever to split (thus, the amount of singletons is always decreasing). I've got this game most of the way 
implemented, but that "GroupSwap" method doesn't work. 

	(define "DistanceFromNearestNeighbor"
		(min
			(results
				from:#1
				to:(intersection (sites LineOfSight at:(from)) (sites Occupied by:Mover))
				(count Steps Orthogonal (from) (to))
			)
		)
	)
	
	(define "AyuMovement"
		(move Step Orthogonal
			(to if:(and 
				(is Empty (to)) 
				(> ("DistanceFromNearestNeighbor" (from)) 
				("DistanceFromNearestNeighbor" (to))))))
	)
	
	(define "GroupSwap"
		(move (from (sites Occupied by:Mover))
			(to if:(is Empty (to)))))
				
	
	//------------------------------------------------------------------------------
	
	(game "Ayu" 
		(players 2) 
		
		(equipment { 
			(board (square 11)) 
			(piece "Disc" Each (or {"GroupSwap" "AyuMovement"}))})
		
		(rules 
			(start { 
				(place "Disc1" {"A2"  "C2"  "E2"  "G2"  "I2"  "K2" 
								"A4"  "C4"  "E4"  "G4"  "I4"  "K4"
								"A6"  "C6"  "E6"  "G6"  "I6"  "K6"
								"A8"  "C8"  "E8"  "G8"  "I8"  "K8"
								"A10" "C10" "E10" "G10" "I10" "K10"})
				(place "Disc2" {   "B1"  "D1"  "F1"  "H1"  "J1"   
								"B3"  "D3"  "F3"  "H3"  "J3"  
								"B5"  "D5"  "F5"  "H5"  "J5"  
								"B7"  "D7"  "F7"  "H7"  "J7"  
								"B9"  "D9"  "F9"  "H9"   "J9" 
								"B11" "D11" "F11" "H11"  "J11"})})
			
			(play (forEach Piece))
			
			(end 
				(if 
					(or {
						(> 2 (count Groups Adjacent if:(= (id Next) (who at:(to)))))
						(no Moves Mover)
					})
					(result Mover Win)
				)
			) 
		)
	)
  
- Catchup

One can place either one or two nodes on their turn, and if one creates a new largest 
group within their turn, the other player may lay down a third. It works, except for the "VariableUpdate"
method. 

	(define "LargestGroupCascading"
		(max (intersection (difference (sizes Group #1) (sizes Group #2)) 
						(sizes Group #1))) 		
	)
	
	(define "VariableUpdate"
		(and {
			(if (<=  (var "MaxGroup") (max (sizes Group Next))) (set Score Mover 1))
			(if (>   (var "MaxGroup") (max (sizes Group Next))) (set Score Mover 0))
			(set Var "MaxGroup" (max (sizes Group Each)))
			
		})
	)
	
	
	//------------------------------------------------------------------------------
	
	(game "Catchup" 
		(players 2) 
		(equipment { 
			(board (hex 5))
			(piece "Marker" Each) 
		}) 
		(rules 
			(play (and {(if (= 0 (count MovesThisTurn)) (do ("VariableUpdate"))) 
						(or {(if (> 2 (count MovesThisTurn))
								(or {(move Add (to (sites Empty)))
									(move Pass)})
									(then (moveAgain)))
							(if (and {(= 1 (var "ChangedLast")) (= 2 (count MovesThisTurn))})
								(or {(move Add (to (sites Empty)))
									(move Pass)}))})}))        
			(end
				(if (is Full)
					(byScore {
						(score P1 ("LargestGroupCascading" P1))
						(score P2 ("LargestGroupCascading" P2))
					}) 
				)
			)
		)
	)
  
   - Ordo 
  
  Like Breakthrough, one starts on top or bottom of the board, and the goal is to 
  get to the other one. The nodes begin as a contiguous group (where diagonal connections count), and 
  must remain that way throughout. All of this is fine and works. What I was unable to implement was the 
  "Ordo Move", a simultaneous movement of any vertical line of nodes horizontally, or horizontal line vertically.  
  The below code works, but it's not quite the full Ordo, as those moves are absent. 
  
  	(define "ReachedTarget" (is In (last To) (sites Mover)))
	
	//------------------------------------------------------------------------------
	
	(game "Ordo" 
		(players 2) 
		(equipment { 
			(board (rectangle 8 10)) 
			(piece "Marker" Each                
				(move Step
					Adjacent
						(to 
							if:(and {(or {(is Empty (to)) ("IsEnemyAt" (to))})
									(< 0 (count Sites in:(sites Around (to) Own Adjacent)))                
									(> 2 (count Groups Adjacent if:(= (who at:(to)) Mover)))}))))
			(regions P1 (sites Top))
			(regions P2 (sites Bottom))})
			
		(rules 
			(meta (no Suicide))
			(start { 
				(place "Marker1" {"A2" "A3" "B2" "B3" "C1" "C2" "D1" "D2" "E2" "E3" "F2" "F3" "G1" "G2" "H1" "H2" "I2" "I3" "J2" "J3"})
				(place "Marker2" {"A6" "A7" "B6" "B7" "C7" "C8" "D7" "D8" "E6" "E7" "F6" "F7" "G7" "G8" "H7" "H8" "I6" "I7" "J6" "J7"})
			})
			(play (forEach Piece))
			
			(end 
				(if 
					(or {
						("ReachedTarget") 
						(no Moves Next)
						(> 1 (count Groups Adjacent if:(= (id Next) (who at:(to)))))					
					})
					(result Mover Win)
				)
			) 
		)
	)
