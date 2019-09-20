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

    // Complete the chocolateFeast function below.
    def chocolateFeast(n: Int, c: Int, m: Int): Long = {

        def recc(ch: Int, m: Int,  s: Long): Long = {
            (ch, m) match {
                case (ch, m) if ch < m => s
                case (ch, m) if ch >= m =>val d = ch/m; recc(d+(ch%m), m, s+d)
            }
        }
        val ch = n/c
        recc(ch, m, ch.toLong)
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val t = stdin.readLine.trim.toInt

        for (tItr <- 1 to t) {
            val ncm = stdin.readLine.split(" ")

            val n = ncm(0).trim.toInt

            val c = ncm(1).trim.toInt

            val m = ncm(2).trim.toInt

            val result = chocolateFeast(n, c, m)

            printWriter.println(result)
        }

        printWriter.close()
    }
}
