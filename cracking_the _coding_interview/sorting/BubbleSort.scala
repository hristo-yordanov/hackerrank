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

    // Complete the countSwaps function below.
    def countSwaps(a: Array[Int]) {
        def swap(_a: Array[Int], _i: Int, _j: Int, _s: Int) : Int = {
            val h = _a(_i)
            _a(_i) = _a(_j)
            _a(_j) = h
            _s + 1
        }

        var s = true //swapped
        var lio = 0 //length index optimizer
        var swaps = 0
        while(s) {
            s = false;
            for (i <- 1 to a.length - lio -1) {
                if (a(i-1) > a(i)) {
                    swaps = swap(a, i-1, i, swaps)
                    s = true;
                }
            }
            lio += 1
        }
        println(s"Array is sorted in $swaps swaps.")
        println(s"First Element: ${a(0)}")
        println(s"Last Element: ${a(a.length-1)}")
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val n = stdin.readLine.trim.toInt

        val a = stdin.readLine.split(" ").map(_.trim.toInt)
        countSwaps(a)
    }
}
