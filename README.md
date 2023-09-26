# 4-levels-of-linear-programming

## Introduction

You have been contacted by the FSharp Corporation, a thriving business that 
ships FSharp all over countries of the European Union.  

Each country needs a steady monthly supply of FSharp. A smaller country like 
Austria needs to receive 10 units of F# each month, whereas a larger country 
like Germany needs 57 units of F# each month.  

The FSharp Corporation operates 5 factories, spread out in the European Union. 
Each factory can produce a limited quantity of F# units every month, and is 
assigned a list of countries it can ship to.  

However, due to an increase in F# demand, Management has noticed that there 
were issues with producing and shipping units where they are needed. Your task 
is to analyze the setup, and optimize it!  

- data.csv file
- data.fsx file

## Level 1: Feasibility

We have 5 factories.  
Each factory has 
- a capacity (150 units), and 
- a list of countries it can ship to.  

The total demand across countries is 563 units, we can produce 5 * 150 units.  
Can we actually meet the demand?  

## Level 2: Linear Programming / Allocation

We start from the same setup as Level 1, but we want to figure out what is the 
most profitable way we can ship units with our current factories.  

Changes from Level 1:

- Every factory now has a Location, the country where it is located.  
- When we ship and sell a unit from a factory to a destination country, we 
receive $1.0, but we incur costs based on the shipping distance.

As a result, the solver should try to favor shipping from closer factories, 
because its objective is to maximize profit.   

## Level 3: Mixed Integer Linear Programming

We start from a setup similar to Level 2, but we want to change our factories 
to become more profitable. We allow two changes: 

- we can now ship from any factory to any country,
- we can change the size of each factory. We assign 0 to 10 machines to each 
factory, with each machine costing 10 but bringing a capacity of 50 units.

## Level 4: Mixed Integer Linear Programming / Traveling Salesman

This problem is mostly independent from Level 1 to 3.

We take on the classic Traveling Salesman Problem, and try to find the shortest 
route that visits all our cities, once, and returns to the origin.

## References

Google OR Tools:  
https://developers.google.com/optimization

FLIPS:  
https://flipslibrary.com

Formulating Integer Linear Programs: A Rogues’ Gallery  
https://faculty.nps.edu/dell/docs/Formulettes060425.pdf

https://cs.stackexchange.com/questions/12102/express-boolean-logic-operations-in-zero-one-integer-linear-programming-ilp

https://math.stackexchange.com/questions/2500415/how-to-write-if-else-statement-in-linear-programming