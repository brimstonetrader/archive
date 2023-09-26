This week was challenging but ultimately quite nice. I decided to add a function to display an article at
the end of Ground Truth, a concatenation of sentences based on the particular hints that they select. I 
had imagined that it would take not too long. I'd worked on it for a total of about three hours when I 
was convinced it was done, as all the necessary sharing of information seemed to have occured. However, 
there was a problem with the way that evidence was being named. In the GameManager, the methods relating
to evidence maintenance mostly look like this:

		public static bool CheckEvidence(string evi){
			return evidence.ContainsKey(evi);
		}
		
With (string evi) being one of "Evi1", "Evi2", ... "Evi8". These refer to the locations in which evidence
may be stored, which get assigned by the game as the player progresses, with "Evi1" being the first piece
of evidence found, and so on. The map is from these strings to booleans, depending on whether or not 
they've been selected. This update does not impact the score attached to each location, which means that
up to now the scoring has not worked. It only becomes a problem when you go through the game non-linearly,
and also play it aiming to win. I thought I'd noticed this a couple of weeks ago, but I convinced myself I
was wrong and moved on. For example, if you were to go first to St. Louis, get as evidence "Louisiana 
Purchase" and "War in 1812", you would get a score of zero, when you should have a score of fifty. These 
two problems turned out to be more connected than I had thought; their solutions were very similar. 

We will be presenting the game on Wednesday to the Studio, and ideally moving into a new stage of 
development, focusing more on things like aesthetics and dialogue than mechanics. I've put a lot of work 
in so far, pleasant, creative mental-work. I'm excited for what's to come. My current method does the 
job, but does not result in a very cohesive or natural-sounding article. Now that I've paved the road, 
though, I can walk it. Next week I hope to work on that, refactor, and possibly add some new characters,
so that the player may take multiple points of view into account before deciding on what they believe to
be true: my long-term political goal is to change truthfulness from a boolean to a gradient. I'm 
interested in fine-tuning the NPC AI so that the player does not get crowded, as I've noticed that to be
somewhat common, but I'm also excited to be spending a good deal of time writing English - no shade to C#.
