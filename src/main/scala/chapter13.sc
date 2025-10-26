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

// Exercise 5
val a = Array(1, 2, 7, 9)
a.mkString("<", ",", ">")
def makeAStr(myArray: Array[Int], front: String, sep: String, end: String): String = {
  val strArray = myArray.map(_.toString)
  front + strArray.reduceLeft(_ + sep + _) + end
}
makeAStr(a, "<", ",", ">")

// Exercise 6
val lst = List(1, 2, 3, 4, 5)
lst.foldRight(List[Int]())(_ :: _).reverse
lst.foldLeft(List[Int]())(_ :+ _)

// Exercise 7
val prices = List(5.0, 20.0, 9.95)
val quantities = List(10, 2, 1)
prices.zip(quantities).map(_ * _)
def multDoubleInt(x: Double, y: Int): Double = {
  x * y
}
prices.zip(quantities).map(multDoubleInt)
val f: (Double, Int) => Double = (x, y) => x * y
val fTupled: ((Double, Int)) => Double = f.tupled
prices.zip(quantities).map[Double](fTupled)

// Exercise 8
val myArray = Array(1, 2, 3, 4, 5, 6)
def make2dArray(arrayInt: Array[Int], cols: Int): Array[Array[Int]] = {
  val iter = arrayInt.grouped(cols)
  var myArray = Array[Array[Int]]()
  while iter.hasNext do
    myArray = myArray :+ iter.next()
  myArray
}
make2dArray(myArray, 3)

// Exercise 9
(1 to 10).flatMap(i => (1 to i).map(j => i * j))

// Exercise 10
val myList: List[Option[Int]] = List(Some(1), None, Some(2), Some(3))
def addOptions(myList: List[Option[Int]]): Int = {
  myList.flatten.sum
}
addOptions(myList)