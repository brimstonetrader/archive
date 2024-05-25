RISE OF QUANTUM

The third-greatest technological buzzword of our time, after "data" and "AI," is swiftly becoming "quantum." What separates
"quantum physics" from the Standard Model? With that, how will computation be revolutionized? How am I effected, or worse,
threatened?

Physics, in general, owes a great deal to the Calculus. The universe is essentially infinite, and tools like limits, 
derivatives, and so on that take this infinitude for granted work well to analyze real-world events. However, at a 
sufficiently small scale, the universe is finite, and phenomena spring from the interactions of a countable quantity of 
atomic particles. This realm requires a totally different mathematical backdrop: that of probability. Two particular 
phenomena which are key to models of quantum computation are "superposition" and "entanglement."

"Superposition" is the phenomenon featured in the well-known "Schrodinger's Cat" thought experiment. Essentially, it allows
for a single particle to exist in multiple states at once, until it is measured. The computational equivalent of a particle
is a "bit," or boolean value, which can be either a zero or a one. In a quantum computation, the particle equivalent is a
"qubit," which can be only a zero or one once measured, but can be put into a superposition with any probability 
distribution (for example, a 70% chance of zero, and 30% chance of one). The particle, remaining in both states, will be in
a superposition until the bit "collapses." This is not an exceptionally hard thing to compute: classical computers can do a
great job of generating random numbers already. Things become interesting with entanglement.

Quantum Entanglement occurs when two particles become linked, classically by emitting two photons in opposite directions 
near each other, at which point they spin in precisely opposite directions. Qubits are able to entangle with each other,
which allows their probability distributions to include dependent as well as independent variables. The simplest qubit 
entanglement is called a "Hadamard Gate," which connects two input qubits, making it so that when they are observed they 
have 50/50 probabilities of collapsing to "00" or "11." To render a computation on some amount x of qubits with a regular
computer, one must track all possible dependencies and superpositions of each qubit, which become very complicated very 
quickly: a standard laptop can only render computations of around 26 qubits.

The algorithms that can be written for a quantum computer offer new, much faster operations to programmers. In 1994, 
Mathematician Peter Shor developed a quantum algorithm which can find the prime factors of a number very quickly, taking a
number of steps that scale with the length of a number, not its size (in more formal terms, Shor's algorithm is O(log(n)^3),
where n is the number of bits used to represent a number n). Factoring is a common problem, and the fastest "classical" 
algorithm is much slower. It is so slow, in fact, that one of the most common cryptography algorithms, called RSA, relies
fundamentally on the sharply increasing complexity of factoring large numbers to encode data securely. 

Encryption is all about finding problems that are much easier to verify than to find a solution to. There are many other
schemas which rely on problems that are no easier for quantum than classical computers, but RSA just multiplies two large
prime numbers together, allowing the product to be a "public key." Estimates vary on the precise number of qubits that
would be needed to factor the 2048-bit numbers used in RSA encryption, but it is almost certainly more than is currently
available. Despite this, many governments and companies worldwide have begun preparing for "Q-Day," re-encrypting old files
with stronger, quantum-resistant methods. There will surely be extremely important documents missed by this, however, as
RSA has been in use for almost 50 years. 

Qubits rely on atomic phenomena, which means that it is concievable that they could be made very small, and one could make
a computer composed of many of them. The largest existing quantum computers hover around 1,000 qubits, but suffer from 
frequent errors due to "quantum noise." Q-Day cannot come until quantum error correction is solved, and the number of
qubits available for one computation is drastically increased. Some say these goals will be reached as early as 2025, but
most estimates range from 10-20 years from now. 

Currently, quantum development is in a state of adolescence, similar to digital computers in the 1940s. Likely, breaking
the most common encryption protocol will be merely the first of many fundamental changes that quantum mechanics brings to
our computers. There is no need to fear it, though, as you have plenty of time and alternative algorithms to get all of
your private documents in order. 
