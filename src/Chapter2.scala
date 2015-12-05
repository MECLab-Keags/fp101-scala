object Chapter2{
    def abs(x : Int) : Int =
      if(x < 0) -x
      else x

    def formattedAbs(x : Int) : String = {
        val msg = "The absolute number of %d is %d"
        msg.format(x, abs(x))
    }

    def factorial(x : Int) : Int = {
        @annotation.tailrec
        def go(n : Int, accumulation : Int): Int =
            if(n <= 0) accumulation
            else go(n-1, n * accumulation)

        go(x, 1)
    }

    /** */
    // 0,1,1,2,3,5
    def findFibonacci(x : Int) : Int = {
        @annotation.tailrec
        def go(counter : Int, target : Int, preceding : Int, current : Int) : Int = {
            val fib = preceding + current
            if(counter == target) fib
            else go(counter + 1, target, current, fib)
        }

        go(0, x, 0, 1)
    }
}

