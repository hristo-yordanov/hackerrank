import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Solution {

    // Complete the rotLeft function below.
    def rotLeft(a: Array[Int], d: Int): Array[Int] = {
        def rotateLeft[A](seq: Seq[A], i: Int): Seq[A] = {
            val size = seq.size
            seq.drop(i % size) ++ seq.take(i % size)
        }
        rotateLeft(a.toList, d).toArray
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val nd = stdin.readLine.split(" ")

        val n = nd(0).trim.toInt

        val d = nd(1).trim.toInt

        val a = stdin.readLine.split(" ").map(_.trim.toInt)
        val result = rotLeft(a, d)

        printWriter.println(result.mkString(" "))

        printWriter.close()
    }
}
