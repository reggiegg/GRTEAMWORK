README.txt file documenting a CPSC 311 programming project.

Please fill out each TODO item in the header but change nothing else,
particularly nothing before the colon ":" on each line!

=================== HEADER =======================

Student #1, Name: Reginald Gillett
Student #1, ugrad.cs.ubc.ca login: w6k8
Student #1, ID: 27133123

Student #2, Name: Graham St-Laurent
Student #2, ugrad.cs.ubc.ca login: i5l8
Student #2, ID: 23310121

Team name (for fun!): GR8Team'); DROP TABLE Teams; --

Project: A2: First Interpreter

Acknowledgment that you understand and have followed the course's
collaboration policy (READ IT at
http://www.ugrad.cs.ubc.ca/~cs311/current/_syllabus.php#Collaboration_Policy):

Signed: Reginald Gillett Graham St-Laurent

=================== BODY =======================

Plese fill in each of the following:

Approximate hours on the projet: 13

Acknowledgment of assistance (per the collab policy!): None

For teams, rough breakdown of work: we pair programmed the whole thing, so, pretty well 50/50




Question 1: Why did we disallow thunks in the concrete syntax for the
lazy interpreter?  In your answer, give an example of a VERY SMALL
program that uses and/or applies thunks and might behave differently
in the lazy version than the user would expect.  Hint: don't forget
there's a strictness point at the interpreter's output; think about
what trouble that or other strictness points might cause.

(SIDE NOTE: In Haskell, a language with lazy evaluation, you can
create a lambda expression equivalent to {fun {x y} {+ x y}} with the
syntax (\x y -> x + y).  Thunks like (\ -> 5) are not valid Haskell
programs, however, nor is there a way to apply a function to zero
arguments.  Now can you see why?  No need to answer this question in
the README!)

We disallowed thunks in the concrete syntax, because the strictness points force
thunks to be evaluated to their CFWAE-Value. In essence, the strictness points
eliminate thunks. So in a function like:

{with {x 5} {{fun {} {+ 3 x}}}}

the thunk gets evaluated to {numV 8}, which is then tried to be applied as:

{{numV 8}}

which is not a valid function, and throws an error.



Question 2: Had we included with* in this assignment, it would be
tempting to translate

  {with* {{x <exp1>} {y <exp2>}} <body>} 

into 

  {{fun {x y} <body>} <exp1> <exp2>}

However, this would NOT in general work correctly.  Fill in the
missing pieces of this sample program (everything in angle brackets)
with code that gives different output for the with* version vs. the
fun version using the semantics of with* from the last assignment.
(Hint: what environment is <exp2> evaluated in?)

{with* {{x 3} {y x}} y}  -> 3
{{fun {x y} y} 3 x}      -> error: "unbound identifier"


Question 3: What's something cool, interesting, frustrating, or
tangentially relevant that this assignment made you think about?

1. We thought about how to create closures in a stack-based language
such as Forth. How would we implement that? Is there even an equivalent
to lazy-evals that is possible in this type of language?





2. If you say strict over and over it loses all meaning. Strict.
Striiiicct. Strict. Strict.
