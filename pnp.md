# P vs. NP

# Educated Guesses From The Hendrix Faculty 3

People have studied problem-solving strategies for millenia, but the organization of such
strategies into computational processes known as algorithms has taken on particular
importance in the last century or so, in light of our "Digital Revolution." For any problem, or,
"input," there exists some algorithm capable of solving it, or producing an "output." The
complexity of such an algorithm is measured by how many steps such an algorithm takes. Two
among many categories computer scientists sort algorithms into are the "polynomial time"
algorithms, which scale to larger versions of the same problem in small, reasonable amounts of
time, and "exponential time" algorithms, which spiral out of control quickly, and are difficult to run
on large, real-world problems.

From these two concepts springs the most essential unsolved problem in theoretical computer
science. A problem (for example, a Sudoku grid) falls into the category of problems known as
"NP" if its solution can be verified by a "polynomial time" algorithm, or, informally, "quickly." If
someone hands you an attempted Sudoku solution, you only need to check each filled-in square
once to make sure it's been solved correctly. The amount of time to check a Sudoku of a
particular size doesn't grow that much for grids larger than 9x9, making this class of problems
verifiable quickly. The unsolved question here is whether or not the fact that a problem belongs
to category "NP", always implies that that problem belongs to another category of problems,
called "P". This category means that the problem is then also solvable quickly, by some
polynomial time algorithm. A proof either way, that NP always implies P, or that NP doesn't
always imply P, would have massive ramifications for any situation involving any sort of
algorithm. I spoke with Hendrix Computer Science Professors Brent Yorgey and Mark Goadrich,
to get some opinions on this critical unsolved question backed up by experience.

- GOADRICH:

I think P is not equal to NP. If it was, we would have found a way by now, but the problem is, by
nature, inconclusive. This, to me, makes the field of CS far more interesting. Having an open
problem at the core of a discipline exposes the limitations that formalization can impose on the
discipline's process of problem solving. I am reminded of Godel's Incompleteness Theorem,
which proves that there can be no fully coherent mathematical system, that is, paradoxes must
always show up. The idea that computers have hard, unshakeable limits, like humans do, gives
me hope for a future that must always be reliant on both styles of problem solving, that is,
human-style, dynamic, and computer-style, methodical.

- YORGEY:

I believe that P does not equal NP. Intuitively, I don't see it, although intuition isn't always
accurate in this field. Further, there are hundreds of NP-Complete problems, such as the
"Traveling Salesman" or "Boolean SAT Problem" which have had decades of intensive effort put
into them, with no polynomial-time algorithm for any in sight. If even one was solved, then all
would be possible, and P would equal NP, but none have been yet found. Maybe we aren't
clever enough as a species to determine this, but I take the absence of proof as evidence that
there is something fundamental at play. If P=NP was formally disproven, it would be a historic
day, but life probably wouldn't change all that much. If it were proven, almost all encryption
would instantly break, and most industrial algorithms would have to be rebuilt from the ground
up. I see a third option, however, where P does equal NP, but the polynomials get so arbitrarily
large that the proof doesn't impact much at all, in the real world. Our algorithms are getting
good, for problems like "SAT," with rarer and rarer cases taking exponential time. I have hope for
the future of computing, but I think this essential limit will always remain.
