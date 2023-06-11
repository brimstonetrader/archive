For a while I'd go from class to class with a deck of cards in my pocket, aces through fours on top. 
During the five minutes between, when us students were anxiously stirring in their seats,
I'd get whoever was sitting next to me to play a game I'd learned as "Anxiety." You lay all the cards
out visible at the beginning, then whoever goes first has to put the ace of spades off to the side, and
choose the card the next player has to lay down. Play carries on like that, and eventually a 4x4 grid
forms (it's illegal to break out of the grid once it exists). The winner is whoever forms a line of 
four cards which share an attribute: Odd/Even, Black/Red, Small/Large, or Sharp On Top/Dull On Top. 

This game exists in Ludii! It's called Quarto, and it uses pieces instead of cards. Nevertheless, I am 
currently using it as a base for the Nigérien trick-taking card game Agram. 

		(game "Agram"
			(players 2)
			(equipment {
				(board (rectangle 2 1) use:Vertex)
				(piece "Square" Each)
				(hand Each size:6)
			})
			(rules 
			
				(start {
					(place "Square1" (handSite P1   ) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square1" (handSite P1  1) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square1" (handSite P1  2) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square1" (handSite P1  3) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square1" (handSite P1  4) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square1" (handSite P1  5) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square2" (handSite P2   ) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square2" (handSite P2  1) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square2" (handSite P2  2) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square2" (handSite P2  3) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square2" (handSite P2  4) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(place "Square2" (handSite P2  5) state:(value Random (range 0 3)) value:(value Random (range 0 8)))
					(forEach Value min:0 max:5 (set Hidden at:(handSite P1 (value)) to:P2))
					(forEach Value min:0 max:5 (set Hidden at:(handSite P2 (value)) to:P1))
					(set Hidden What (sites Hand P1) to:P2)
					(set Hidden What (sites Hand P2) to:P1)
				}      )
				
				phases:{
				(phase "Lead" 
					(play 
						(if (= (id P1) (mover))
						
							(move  
								(from (sites Hand Mover))
								(to (sites Top))
								(then (do (set Score Mover (+ (* (state at:(to)) 9) (value Piece at:(to))))))
							)
							(move  
								(from (sites Hand Mover))
								(to (sites Bottom))
								(then (do (set Score Mover (+ (* (state at:(to)) 9) (value Piece at:(to))))))
							))
					)
					(nextPhase "Follow")
				)
				(phase "Follow" 
					(play 
						(or {
							(do (fromTo  
								(from (sites Hand Mover)) 
								(to (sites Empty))
								(then (do (set Score Mover (+ (* (state at:(to)) 9) (value Piece at:(to))))))) 
								ifAfterwards:(or { (is Line 2 All whats:{(id "Square" P1) (id "Square" P2)} if:(= 0 (state at:(to))))
												(is Line 2 All whats:{(id "Square" P1) (id "Square" P2)} if:(= 1 (state at:(to))))
												(is Line 2 All whats:{(id "Square" P1) (id "Square" P2)} if:(= 2 (state at:(to))))
												(is Line 2 All whats:{(id "Square" P1) (id "Square" P2)} if:(= 3 (state at:(to))))}))
							
							(if (< (forEach Site (sites Hand Mover) 
											if:(!= (/ (score Prev) 9) (state at:(site))) 0) 1)
								(move  
								(from (sites Hand Mover)
									(to (sites Empty))
									(then (do (set Score Mover (+ (* (state at:(to)) 9) (value Piece at:(to))))))
								)))})				
					)
					(nextPhase (is Mover P2) "Trick")
				)
				(phase "Trick" 
					(play
						(move 
							(from Cell (last From))
							(to (sites Hand Mover))
						)
					)
					(end { 
						// Shape line (what)
						(if (is Line 4 All whats:{(id "Square" P1) (id "Square" P2)}) (result Mover Win))
						(if (is Line 4 All whats:{(id "Square" P1) (id "Square" P2)}) (result Mover Win))
						
						// Colour line (who)
						(if (is Line 4 All P1) (result Mover Win))
						(if (is Line 4 All P2) (result Mover Win))
						
						// State line (state)
						(if (is Line 4 All whats:{(id "Square" P1) (id "Square" P2) (id "Square" P1) (id "Square" P2)} if:(= 0 (state at:(to)))) (result Mover Win))
						(if (is Line 4 All whats:{(id "Square" P1) (id "Square" P2) (id "Square" P1) (id "Square" P2)} if:(= 1 (state at:(to)))) (result Mover Win))
						
						// Value line (value)
						(if (is Line 4 All whats:{(id "Square" P1) (id "Square" P2) (id "Square" P1) (id "Square" P2)} if:(= 0 (value Piece at:(to)))) (result Mover Win))
						(if (is Line 4 All whats:{(id "Square" P1) (id "Square" P2) (id "Square" P1) (id "Square" P2)} if:(= 1 (value Piece at:(to)))) (result Mover Win))
					})
				)
				}
			)
		)
		
		//------------------------------------------------------------------------------
		
		(metadata 
			
			(info
				{
				(description "Quarto has a 4Ã—4 board and 16 pieces. Each piece has four dichotomous attributes â€“ color, height, shape, and consistency â€“ so each piece is either black or white, tall or short, square or round, and hollow or solid.")
				(rules "Each piece has four dichotomous attributes â€“ color, height, shape, and consistency â€“ so each piece is either black or white, tall or short, square or round, and hollow or solid. The object is to place the fourth piece in a row in which all four pieces have at least one attribute in common. The twist is that your opponent gets to choose the piece you place on the board each turn.")
				(id "1536")
				(source "<a href=\"https://boardgamegeek.com/boardgame/681/quarto\" target=\"_blank\" class=\"style1\" style=\"color: #0000EE\" />BGG</a>")
				(version "1.3.11")
				(classification "board/space/line")
				(author "Blaise Muller")
				(publisher "<a href=\"https://www.gigamic.com/\" target=\"_blank\" class=\"style1\" style=\"color: #0000EE\" />Gigamic</a> (used with permission)")
				(credit "Eric Piette")
				(date "1991")
				}
			)
			
			(graphics {
				(piece Foreground state:0 image:"Heart" fillColour:(colour Red)    edgeColour:(colour Red)   scale:1)
				(piece Foreground state:1 image:"Cross" fillColour:(colour Red)    edgeColour:(colour Red)   scale:1)
				(piece Foreground state:2 image:"Star"  fillColour:(colour Yellow) edgeColour:(colour Yellow) scale:1)
				(piece Foreground state:3 image:"Moon"  fillColour:(colour Yellow) edgeColour:(colour Yellow) scale:1)
				(piece Foreground value:0 image:"9"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:1 image:"8"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:2 image:"7"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:3 image:"6"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:4 image:"5"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:5 image:"4"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:6 image:"3"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:7 image:"2"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(piece Foreground value:8 image:"A"     fillColour:(colour Hidden) edgeColour:(colour Blue) scale:0.4)
				(show Edges Hidden)
				(show Symbol "Square" (sites Board) fillColour:(colour 185 130 85) edgeColour:(colour 225 182 130)) 
				(board Placement scale:0.8)
				(board Background image:"square.svg" fillColour:(colour 185 130 85) edgeColour:(colour 185 130 85) scale:1.45)
			})
			
			(ai
				"Agram_ai"
			)
		)
		
		
