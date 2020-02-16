object Main {

  type BooleanFunction  = Int
  type BooleanArguments = Int

  private def callFunction(function: BooleanFunction, args: BooleanArguments) = function >> args & 1

  val allAssociativeBooleanFunctions: List[BooleanFunction] =
    Range(0, 1 << (1 << 2)).filter { function =>
      def call(args: BooleanArguments): Int = callFunction(function, args)

      Range(0, 1 << 3).forall { argsToCheck =>
        // (xy)z == x(yz)
        val xy = argsToCheck >> 1
        val z  = argsToCheck & 1

        val x  = argsToCheck >> 2
        val yz = argsToCheck & 3

        call(call(xy) << 1 | z) == call(x << 1 | call(yz))
      }
    }.toList

  def main(args: Array[String]): Unit = {
    println("x y | f(x, y)")
    println(List.fill(5 + allAssociativeBooleanFunctions.size * 2)("-").mkString)
    Range(0, 1 << 2).foreach { args =>
      val results = allAssociativeBooleanFunctions.map(callFunction(_, args)).mkString(" ")
      println(s"${args >> 1 & 1} ${args & 1} | $results")
    }
  }

}
