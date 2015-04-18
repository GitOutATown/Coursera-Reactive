package week1

object funs {

  val f: (Int => String) = List("a", "b", "c")    //> f  : Int => String = List(a, b, c)
  f(2)                                            //> res0: String = c
  f(0)                                            //> res1: String = a
  // f(3) // IndexOutOfBoundsException
    
  val fun: PartialFunction[String, String] = { case "ping" => "pong" }
                                                  //> fun  : PartialFunction[String,String] = <function1>
  fun("ping")                                     //> res2: String = pong
  fun.isDefinedAt("ping")                         //> res3: Boolean = true

}