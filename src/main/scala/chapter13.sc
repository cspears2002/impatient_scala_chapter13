// Exercise 1

import scala.collection.*
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