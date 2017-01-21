# Robbie Robot Genetic Algorithm in Scala

Scala implementation of the Robbie Robot problem as described in,
_Melanie Mitchell_, Complexity A Guided Tour.

Primarily written as an exercise in the genetic algorithm. Subsequent UI code added for illustrative purposes.

## Usage
To run:
```
sbt run
```
The most interesting genetic algorithm parameters can be altered in AlgorithmRunner:

The iterationCount is the number of generations the algorithm will run.
It is set to 5 because it doesn't take too long to run. To see it really working, something more like 200 is needed, although interesting results can start to appear by 50 iterations.
```
  val iterationCount = 5
```
In general a higher population size is going to be better, giving more diversity and hence increasing the chances of a good result.
```
  val populationSize = 200
```
The remaining two parameters determine how many boards/turns on a board, will be performed in order to determine a strategies fitness.
```
  val boardCount = 100
  val numberOfTurnsPerBoard = 200
```

## Tests
```
sbt test
```
