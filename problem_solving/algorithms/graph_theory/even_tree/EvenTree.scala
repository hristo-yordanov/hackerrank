import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
import scala.collection.parallel.immutable._
import scala.collection.parallel.mutable._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._
import scala.collection.mutable.Map
import scala.collection.immutable.List

object Solution {

    // Complete the evenForest function below.
    def evenForest(t_nodes: Int, t_edges: Int, t_from: Array[Int], t_to: Array[Int]): Int = {

        def buildStructure(t_edges: Int, t_from: Array[Int], t_to: Array[Int], a: Map[Int, List[Int]] = Map[Int, List[Int]]()): Map[Int, List[Int]] = {
            for (i <- 0 to t_edges - 1) {
                if (a.contains(t_to(i))) {
                a.update(t_to(i), a.getOrElse(t_to(i), List.empty[Int]) :+ t_from(i))
                } else {
                a.put(t_to(i), List(t_from(i)))
                }
            }
            a
        }

        def childs(n: Int, m: Map[Int, List[Int]], a: Int = 0) : Int = {
            m.getOrElse(n, List[Int]()).foldLeft(a)((ac, x) => {
                val l = m.getOrElse(x, List[Int]())
                if(l.isEmpty) {
                ac + 1
                }
                else childs(x, m, ac+1)
            })
        }

        val m = buildStructure(t_edges, t_from, t_to)
        m.keys.filter(k => k != 1).foldLeft(0)((a, k) => {
            if(childs(k, m) % 2 == 0) a
            else a + 1
        })
    }

    def main(args: Array[String]) {
        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        var Array(tNodes, tEdges) = StdIn.readLine().replaceAll("\\s+$", "").split(" ").map(_.toInt)

        val tFrom = Array.ofDim[Int](tEdges)
        val tTo = Array.ofDim[Int](tEdges)

        for (i <- 0 until tEdges) {
            val tFromTo = StdIn.readLine().replaceAll("\\s+$", "").split(" ")

            tFrom(i) = tFromTo(0).toInt
            tTo(i) = tFromTo(1).toInt
        }

        val res = evenForest(tNodes, tEdges, tFrom, tTo)
        //val res = evenForest(3, 3, Array(1,2), Array(1,2))

        printWriter.println(res)
        //printWriter.println(tNodes)

        printWriter.close()
    }
}
