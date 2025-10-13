// Exercise 1

import scala.collection.*
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

import java.lang.System

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

def removeEven(myListBuffer: ListBuffer[Int]): ListBuffer[Int] = {
  myListBuffer.zipWithIndex.reverse.foreach(
    (c, i) => if i % 2 == 0 then myListBuffer.remove(i))
  myListBuffer
}
val listBuf = ListBuffer(0, 1, 2, 3, 4, 5)
removeEven(listBuf)

def copyEvenToList(myListBuffer: ListBuffer[Int]): ListBuffer[Int] = {
  val newListBuffer = ListBuffer[Int]()
  myListBuffer.zipWithIndex.reverse.foreach(
    (c, i) => if i % 2 == 0 then newListBuffer.append(c)
  )
  newListBuffer
}
val listBuf1 = ListBuffer(0, 1, 2, 3, 4, 5)
removeEven(listBuf1)

def profile[R](code: => R, description: String = "Function"): (R, Long) = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  val timeTaken = end - start
  println(s"$description executed in ${timeTaken / 1_000_000.0} ms")
  (result, timeTaken)
}
val listBuf2 = ListBuffer(0, 1, 2, 3, 4, 5)
val (result1, time1) = profile(removeEven(listBuf2), "Remove items")
val (result2, time2) = profile(removeEven(listBuf2), "Copy to list")

// Exercise 4
def getNameInts(names: Array[String], nameToInt: Map[String, Int]): Array[Int] = {
  names.flatMap(name => nameToInt.get(name))
}
val names = Array("Tom", "Fred", "Harry")
val nameToInt = Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)
val arrayInts = getNameInts(names, nameToInt)
