import scala.io.StdIn
import scala.collection.mutable.Stack

object Solution {

    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
        var stack = Stack[String]()
        var s = ""
        val operations = StdIn.readInt()
        1 to operations foreach { _ =>
            StdIn.readLine.split(" ") match {
                case Array("1", t) => {stack.push(s); s = s+ t}
                case Array("2", t) => stack.push(s); s = s.dropRight(t.toInt)
                case Array("3", t) => println(s(t.toInt-1))
                case Array(_) => s = stack.top; stack.pop()
            }
        } 
    }
}


