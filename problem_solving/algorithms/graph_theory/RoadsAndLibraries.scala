import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.mutable.Map
import scala.collection.immutable.List
import scala.collection.immutable.Set

object Solution {

    // Complete the roadsAndLibraries function below.
    def roadsAndLibraries(n: Int, c_lib: Long, c_road: Long, cities: Array[Array[Int]]): Long = {
        //v - visited
        //c - cities
        //n - node (specific city)
        def traverse(v: Set[Int], n: Int, g: Map[Int, Set[Int]], c: Int) : (Set[Int], Int) = {
            if (v.contains(n)) (v, c)
            else {
                g.getOrElse(n, List[Int]()).foldLeft((v+n, c))((ac, x) => {
                    if(ac._1(x)) ac
                    else traverse(ac._1, x, g, ac._2 + 1)
                })
            }
        }

        //build an adjacency map
        def loadAdjacencyMap(as: Array[Array[Int]]) : Map[Int, Set[Int]] = {
        as.map( { case Array(a,b) => (a, b) })
            .foldLeft(Map.empty[Int, Set[Int]]){ (m, t) => {
                if(m.contains(t._1)) m.update(t._1, m.getOrElse(t._1, Set.empty[Int]) + t._2 )
                else m.put(t._1, Set(t._2))
                if(m.contains(t._2)) m.update(t._2, m.getOrElse(t._2, Set.empty[Int]) + t._1 )
                else m.put(t._2, Set(t._1))
                m
            }}
        }

        if(c_lib > c_road){
            val g = loadAdjacencyMap(cities)
            //travers every node if not already visited and return visited nodes
            //visited, libraries, paths
            val r = g.foldLeft((Set[Int](), 0, 0)) { case (a, (k, v)) =>
                if(a._1.contains(k))  a
                else {
                val t = traverse(a._1, k, g, 0)
                //1 - visited, 2 - libraries, 3 - cities
                (t._1 union a._1, a._2 + 1, a._3 + t._2)
                }
            }
            ((n - g.size)*c_lib) + (r._2*c_lib) + (r._3*c_road)
        } else {
            n*c_lib
        }
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val q = stdin.readLine.trim.toInt

        for (qItr <- 1 to q) {
            val nmC_libC_road = stdin.readLine.split(" ")

            val n = nmC_libC_road(0).trim.toInt

            val m = nmC_libC_road(1).trim.toInt

            val c_lib = nmC_libC_road(2).trim.toInt

            val c_road = nmC_libC_road(3).trim.toInt

            val cities = Array.ofDim[Int](m, 2)

            for (i <- 0 until m) {
                cities(i) = stdin.readLine.split(" ").map(_.trim.toInt)
            }

            val result = roadsAndLibraries(n, c_lib, c_road, cities)

            printWriter.println(result)
        }

        printWriter.close()
    }
}

