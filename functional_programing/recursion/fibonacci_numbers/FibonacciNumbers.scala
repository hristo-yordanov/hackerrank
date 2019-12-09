import scala.io.StdIn.readInt

object Solution {

  def fibonacci(x:Int):Int = {
    def f(x:Int, l:Int = 0, r:Int = 1):Int = x match {
      case 1 => r
      case _ => f(x-1, r, l+r)
    }
    f(x-1)
  }

  def main(args: Array[String]) {
    /** This will handle the input and output**/
    println(fibonacci(readInt()))

  }
}