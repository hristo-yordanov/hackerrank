import java.io._ 
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set


object Solution {

    // Complete the journeyToMoon function below.
    def journeyToMoon(n: Int, astronaut: Array[Array[Int]]): Long = {
      val vertices = loadVertices(n)
      val neighbours = loadNeighbours(astronaut)
        //neighbours
      val mx = astronaut.flatten.toSet
        .foldLeft((List[Long](), List[List[Long]]()))((a,v) => {
          if(a._1.contains(v)) a
          else {
            val path = travers(v, neighbours)
            (path ::: a._1, path :: a._2)
        }
      })
      val list = vertices.diff(mx._2.flatten)
        .foldLeft(mx._2) (( acc, v) => List(v) :: acc)
        .map(_.size)
      sumList(list)
    }

    def sumList(list: List[Int]): Long = {
      def fAcc(l: List[Int], accSum: Long, acc: Long): Long = {
        l match {
          case h :: Nil => acc
          case h :: t => val sum = accSum + h; fAcc(t, sum, acc + (sum * t.head))
        }
      }
      fAcc(list, 0L, 0L)
    }


    def travers(vertex: Long, neighbours: Map[Long, List[Long]], 
                v: List[Long] = List[Long]()): List[Long] = {
      if (v.contains(vertex)) v
      else {
        neighbours.getOrElse(vertex, List[Long]()).foldLeft(vertex :: v)((av, n) => {
          if(av.contains(n)) av
          else travers(n, neighbours, av)
        })
      }
    }

    def loadVertices(n: Int): List[Long] = {
      def verticesAcc(i: Int, acc: List[Long]): List[Long] =  {
        i match {
          case i if i < 0 => acc
          case i => verticesAcc(i - 1, i :: acc)
        }
      }
      verticesAcc(n-1, List.empty[Long])
    }

    def loadNeighbours(astronaut: Array[Array[Int]]): Map[Long, List[Long]] = {
      astronaut
        .map( { case Array(a,b) => (a.toLong, b.toLong) })
        .foldLeft(Map.empty[Long, List[Long]]){ (m, t) => {
          val ml = m + ((t._1, t._2 :: m.getOrElse(t._1, List.empty[Long])))
          ml + ((t._2, t._1 :: ml.getOrElse(t._2, List.empty[Long])))
        }
        }
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val np = stdin.readLine.split(" ")

        val n = np(0).trim.toInt

        val p = np(1).trim.toInt

        val astronaut = Array.ofDim[Int](p, 2)

        for (i <- 0 until p) {
            astronaut(i) = stdin.readLine.split(" ").map(_.trim.toInt)
        }

        val result = journeyToMoon(n, astronaut)

        printWriter.println(result)

        printWriter.close()
    }
}

