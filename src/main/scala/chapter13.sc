// Exercise 1

import scala.collection.*
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

def createMapOfIndices(myString: String): mutable.Map[Char, mutable.LinkedHashSet[Int]] = {
  val mapOfIndices = mutable.Map[Char, mutable.LinkedHashSet[Int]]()
  for (c, idx) <- myString.zipWithIndex
    do
      if mapOfIndices.contains(c) then
        mapOfIndices(c) += idx
      else
        mapOfIndices(c) = mutable.LinkedHashSet(idx)
  mapOfIndices
}
val indicesMap = createMapOfIndices("Mississippi")
println(indicesMap)

// Exercise 2

// The c variable is actually a tuple.
def immMapOfIndices(myString: String): immutable.Map[Char, List[Int]] = {
  myString.zipWithIndex.foldLeft(immutable.Map[Char, List[Int]]())((m, c) =>
    m.updated(c(0), m.getOrElse(c(0), List.empty[Int]) :+ c(1)))
}
val immIndicesMap = immMapOfIndices("Mississippi")
println(immIndicesMap)

// Exercise 3

def printFromEnd(myListBuffer: ListBuffer[Int]): Unit = {
  myListBuffer.zipWithIndex.reverse.foreach(
    (c, i) => if i % 2 == 0 then println(c))
}
val listBuf = ListBuffer(0, 1, 2, 3, 4, 5)
printFromEnd(listBuf)