Things are going well with our journalism game! Here's a brief survey of some of the more notable things that I've done so far.

1. Initially, to exit the notebook or investigation area, one had to click on an "x". This is fine, but I made it so that
   pressing "Q" does the same thing in both contexts. This was a simple change to describe, but required rearranging a lot
   of the scene-change infrastructure, so ultimately a lot of that is now more elegantly organized. 

4. I added animations for the main character, allowing for turning in all four directions, and a cute idle animation where
   he's writing in his notebook. In the future, we may want to animate NPCs, so this was a good refresher on that process.

9. Currently, the NPCs wander around the various towns according to a script that relies on "Waypoints". These are
   coordinates which are randomly cycled between, and the NPCs walk in straight lines from one to the next. I added this
   script to the NPCs in a few scenes where they were still, arranging the waypoints.

11. Initially, the game merely displayed the score on completion. I added a dictionary to the GameManager method which
    tracks the articles selected as true, and displays a single string containing all of them on the last page of the
    journal, and on the screen where the player gets their score. This is what I am working on now, it is not fully
    functional yet.

Some future endeavors that might be good for this game are fact-checking, allowing the user to select "how true" a piece of 
evidence is along a gradient instead of as a boolean (True, Mostly True, Indeterminate, Mostly False, False). This would also 
allow for more NPCs,  talking about the same issues from different perspectives. Currently, each piece of evidence has one 
person advocating for or against it, but it is never advisable to report on the general mood on an issue with only one source. 







