package week1

/** This is the complete generators example, the simplified version for the
 *  exam is in the other file.
 */
object Generators {
  println("+++ In outer object Generators")
  
  def main(args: Array[String]) {
    println("\n===> In main")
    
    println("\nBefore invocation of triangles Generator")
    val triangelsGenerator = triangles(20) /* This is an interesting pattern: 
    * At this point we have invoked an arrow (flatMap in a for comprehension, but it 
    * has not yet produced (i.e. fired, iterated) a value. flatMap has only produced 
    * the Generator, and generate must be called on it to produce the actual value.*/
    println("Now calling generate on triangles Generator")
    println("---> triangles: " + triangelsGenerator.generate)
    
    println("\nBefore invocation of lists Generator")
    val listsGenerator = lists 
    /* At this point we have invoked an arrow (flatMap in a for comprehension, but it 
     * has not yet produced (i.e. fired, iterated) a value. flatMap has only produced 
     * the Generator, and generate must be called on it to produce the actual value.*/
    println("Now calling generate on lists Generator")
    println("---> lists: " + listsGenerator.generate)
    
    println("\nBefore invocation of trees Generator")
    val treesGenerator = trees 
    /* At this point we have invoked an arrow (flatMap in a for comprehension, but it 
     * has not yet produced (i.e. fired, iterated) a value. flatMap has only produced 
     * the Generator, and generate must be called on it to produce the actual value.*/
    println("Now calling generate on trees Generator(name:" + treesGenerator.name + ")")
    /* flatMap's def generate calls self.generate to generate a boolean value, "cutoff" 
     * (from booleans) which is then used in..? to..? */
    println("---> trees: " + treesGenerator.generate)
    
    println("\nBefore invocation of test pairs")
    test(pairs(lists, lists)) {
      case (xs, ys) => {
        println("---> test pairs - xs:::ysLength:" + (xs ::: ys).length + " xsLength:" + xs.length + " ys.length:" + ys.length)
        (xs ::: ys).length > xs.length
      }
    }
  } // end main

  trait Generator[+T] {
    self =>
      
    // for diagnostics
    var name: String = ""
          
    println("+++ In trait Generator after self")
    def generate: T // This is the only abstract def, and this def is what distinguishes each of the different types of Generators.
    def foreach[U](f: T => U) {
      println("In train Generator foreach, calling f(generate)")
      f(generate)
    }
    
    /* This map function being called on an existing (parent) Generator creates a new 
     * Generator. Here we are passing in an anon func used in the new def generate. 
     * The new generate def invokes the passed in anon func with a call to the parent
     * generate (self) func to get T which the anon func uses to produce S. 
     * Note that these Generators are recreated everytime these functions are called.
     * Note also that although this is a map func, it is only generating one value at
     * a time via a call to the parent generate func.
     */ 
    def map[S](f: T => S): Generator[S] = new Generator[S] {
      println("In (parent) Generator(name:" + self.name + ") def map constructing new Generator.")
      def generate = {
        println("In trait Generator(name:" + self.name + ") map generate, calling f(self.generate)")
        val self_generate = self.generate
        val f_self_generate = f(self_generate) // invocation of the anon func passed in. T is produced by parent Generator's generate func and the passed in anon func defines what S's type is and how it's produced by this new Generator created by this map function of the parent.
        println("In trait Generator(name:" + self.name + ") map BOTTOM, self_generate:" + self_generate + " f_self_generate:" + f_self_generate)
        //result
        f_self_generate
      }
    }
    
    /* This flatMap function called on an existing (parent) Generator (ex: integers, booleans) 
     * creates a new Generator. The new generate def invokes the passed in anon 
     * func with a call to the parent (self) generate func to get T which the 
     * anon func uses to produce another new Generator (this time via the map 
     * func above), which it then calls generate on. Note that the second Generator
     * doesn't get created until this generate function is called.
     */
    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      /* Remember that self is not this new Generator being constructed here, but rather the containing/parent
       * Generator (which I'm logging).*/
      println("In (parent) Generator(name:" + self.name + ") def flatMap constructing new Generator.")
      def generate = {
        println("In trait Generator(name:" + self.name + ") flatMap generate<1>, calling self.generate")
        val self_generate = self.generate
        println("In trait Generator(name:" + self.name + ") flatMap generate, self_generate: " + self_generate)
        println("In trait Generator(name:" + self.name + ") flatMap generate<2>, calling f(parent_generated).generate")
        val f_self_generate_generate = f(self_generate).generate
        println("In trait Generator(name:" + self.name + ") flatMap BOTTOM, self.generate:" + self_generate + 
            " f_self_generate_generate:" + f_self_generate_generate)
        // result
        f_self_generate_generate
      }
    }
  } // end trait Generator

  // Constructing new Generator directly and defining generate def to produce random Int.
  val integers = new Generator[Int] {
    name = "integers"
    println("In val integers Generator (name:" + name + ") constructor TOP")
    def generate = {
      println("In def integers def generate of Generator (name:" + this.name + ")")
      scala.util.Random.nextInt()
    }
  }
  println(">>> integers Generator(name:" + integers.name + ")")

  /* integers Generator has been created and here we're calling its map function 
   * with an anon func of type Int => Boolean to produce a new Generator that
   * produces Booleans. 
   */
  val booleans = {
    println("In val booleans TOP before integers.map...")
    val generator = integers.map(_ >= 0) // passing anon func
    generator.name = "booleans"
    println("In val booleans BOTTOM, Generator(name:" + generator.name + ")")
    generator
  }
  println(">>> booleans Generator(name:" + booleans.name + ")")

  def choose(from: Int, to: Int) = new Generator[Int] {
    name = "choose"
    println("In def choose Generator (name:" + name + ") constructor TOP")
    def generate = {
      println("In def choose def generate")
      if (from == to) from else scala.util.Random.nextInt(to - from) + from
    }
  }

  def single[T](x: T) = new Generator[T] {
    name = "single"
    println("In def Generator(name:" + this.name + ") constructor TOP")
    def generate = {
      println("In single generate")
      x
    }
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = {
    println("In def pairs before for...")
    val generator = for {
	    x <- t // flatMap
	    y <- u // map
	} yield (x, y)
	generator.name = "pairs"
	generator
  }

  /* Similar to booleans, we are passing an anon func to
   * integers.flatMap to create a new Generator of Int pairs.
   */
  val pairDesugared: Generator[(Int, Int)] = {
    println("In val pairDesugared TOP before integers flatMap...")
    val generator = integers flatMap {
	    x => integers map { y => (x, y) }
	}
    generator.name = "pairDesugared"
    println("In val pairDesugared BOTTOM, Generator(name:" + generator.name + ")")
    generator
  }

  def test[T](r: Generator[T], noTimes: Int = 100)(test: T => Boolean) {
    println("In def test r:" + r + " noTimes:" + noTimes + " test:" + test)
    for (_ <- 0 until noTimes) { // foreach
      val value = r.generate
      assert(test(value), "Test failed for: "+value)
    }
    println("Test passed "+noTimes+" times")
  }

  def triangles(width: Int): Generator[(Int, Int)] = {
    println("In def triangles before invoking for comprehension")
    val generator = for {
	    x <- choose(0, width) // flatMap, requires call to generate to actually produce a value!
	    y <- choose(0, x) // map
	} yield (x, y)
	generator.name = "triangles"
	generator
  }

  def emptyLists = {
    println("In def emptyLists")
    val generator = single(Nil) // have them implement these
    generator.name += "_emptyLists"
    generator
  }

  def nonEmptyLists = {
    println("In def nonEmptyLists before invoking for comprehension")
    val generator = for {
	    head <- integers // flatMap
	    tail <- lists // map, conditionally recursive
	} yield head :: tail
	generator.name = "nonEmptyLists"
	generator
  }

  def lists: Generator[List[Int]] = {
    println("In def lists before invoking for comprehension")
    val generator = for {
	    cutoff <- booleans // flatMap
	    list <- if (cutoff) emptyLists else nonEmptyLists // map
	} yield list
	generator.name = "lists"
	generator
  }

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def leafs: Generator[Leaf] = {
    println("In def leafs before invoking for comprehension")
    val generator = for {
	    x <- integers // map
	} yield Leaf(x)
	generator.name = "leafs"
	generator
  }

  def inners: Generator[Inner] = {
    println("In def inners before invoking for comprehension")
    val generator = for {
	    l <- trees // flatMap
	    r <- trees // map
	} yield Inner(l, r)
	generator.name = "inners"
	generator
  }

  def trees: Generator[Tree] = {
    println("In def trees before invoking for comprehension")
    /* Wow, this is a little bizarre to me: What I'm seeing from my diagnostic logging is very different
     * from what I would have expected. This is because we are defining map and flatMap behavior that
     * different from what is typical in that these two methods do not fire (produce) values when
     * called, but rather are used to construct Generators on which subsequent calls to generate do then 
     * produce values. The most perplexingly unusual behavior to me right now is that this for comprehension 
     * is able to return the generator created by the flatMap (i.e. <- booleans) without having proceeded 
     * through the rest of the for block, i.e. the map (i.e. <- if (cutoff)). But I am remembering that 
     * flatMap as defined here does not produce any value (and therefor, 'cutoff' cannot yet be assigned, 
     * which means that the line 'cutoff <- booleans' does not complete. But evidently the new Generator 
     * which flatMap does produce is returned/assigned to the 'generator' val. I am now presuming therefore 
     * that the 'cutoff <- booleans' anon func is available to be called within the existing closure scope, 
     * and in fact, it IS the anon func that's being passed in as the flatMap parameter. Remember that there 
     * is a chaining of Generators so look for how the second Generator (map) is created and its generate def 
     * called. I would like to have a clearer vision of how the arrow version of flatMap accepts parameters.*/
    val generator = for {
	    cutoff <- booleans // <- is flatMap, which creates a new Generator (of Booleans). Note that booleans has already been instantiated. Also note that even though flatMap and map are more familiarly (to me) used to iterate through Lists, in this pattern they are only generating one value at a time (via def generate on the Generator they cause to be constructed).
	    tree <- if (cutoff) leafs else inners // <- is map, produces the second Generator, which produces Trees.
	} yield tree
	generator.name = "trees"
	println("Tree Generator(name:" + generator.name + ") has been created from def trees for comprehension.")
	generator
  }
}









