package org.functionalkoans.forscala

import support.KoanSuite

class AboutTraits extends KoanSuite {
  koan("A class uses the extends keyword to mixin a trait if it is the only relationship the class inherits") {
    case class Event(name: String)

    trait EventListener {
      def listen(event: Event): String
    }


    class MyListener extends EventListener {
      def listen(event: Event): String = {
        event match {
          case Event("Moose Stampede") => "An unfortunate moose stampede occurred"
          case _ => "Nothing of importance occurred"
        }
      }
    }

    val evt = Event("Moose Stampede")
    val myListener = new MyListener
    myListener.listen(evt) should be (__)
  }

  koan("A class can only \'extend\' from one class or trait, any subsequent extension should use the keyword \'with\'") {

    case class Event(name: String)

    trait EventListener {
      def listen(event: Event): String
    }

    class OurListener

    class MyListener extends OurListener with EventListener {
      def listen(event: Event) : String = {
        event match {
          case Event("Woodchuck Stampede") => "An unfortunate woodchuck stampede occurred"
          case _ => "Nothing of importance occurred"
        }
      }
    }

    val evt = Event("Woodchuck Stampede")
    val myListener = new MyListener
    myListener.listen(evt) should be (__)
  }

  koan("Traits are polymorphic. Any type can be referred to by another type if related by extension") {
    case class Event(name: String)

    trait EventListener {
      def listen(event: Event): String
    }

    class MyListener extends EventListener {
      def listen(event: Event) : String = {
        event match {
          case Event("Moose Stampede") => "An unfortunate moose stampede occurred"
          case _ => "Nothing of importance occurred"
        }
      }
    }

    val myListener = new MyListener

    myListener.isInstanceOf[MyListener] should be(__)
    myListener.isInstanceOf[EventListener] should be(__)
    myListener.isInstanceOf[Any] should be(__)
    myListener.isInstanceOf[AnyRef] should be(__)
  }

  koan("Traits can have concrete implementations that can be mixed into concrete classes with it's own state") {
    trait Logging {
      var logCache = List[String]()

      def log(value: String) = {
        logCache = logCache :+ value
        println(value)
      }
    }

    class Welder extends Logging {
      def weld() {
        log("welding pipe")
      }
    }

    class Baker extends Logging {
      def bake() {
        log("baking cake")
      }
    }

    val welder = new Welder
    welder.weld()


    val baker = new Baker
    baker.bake()

    welder.logCache.size should be(__)
    baker.logCache.size should be(__)
  }

  koan("""Traits can also be mixed during instantiation after the fact!
          | This is useful if you only want to mixin per instance and not per class""") {

    trait Logging {
      var logCache = List[String]()

      def log(value: String) = {
        logCache = logCache :+ value
      }

      def log = logCache
    }

    class Scientist (val firstName:String, val lastName:String) {
        def discover(item:String) = s"I have discovered $item!"
        def invent(item:String) = s"I have invented $item!"
    }

    val einstein = new Scientist("Albert",  "Einstein") with Logging  //mixin traits during instantiation!
    einstein.discover("Relativity!")
    einstein.log("Although it is utmost of importance that this does not fall into the wrong hands")

    einstein.log.size should be (__)
  }

  //Credit for the next set koans: http://www.artima.com/scalazine/articles/stackable_trait_pattern.html

  abstract class IntQueue {
    def get(): Int
    def put(x: Int)
  }

  import scala.collection.mutable.ArrayBuffer

  class BasicIntQueue extends IntQueue {
    private val buf = new ArrayBuffer[Int]
    def get() = buf.remove(0)
    def put(x: Int) { buf += x }
  }


  koan("Traits are stackable and can change the behavior of methods that the traits are stacked upon") {
    trait Doubling extends IntQueue {
      abstract override def put(x: Int) { super.put(2 * x) } //abstract override is necessary to stack traits
    }

    class MyQueue extends BasicIntQueue with Doubling

    val myQueue = new MyQueue
    myQueue.put(3)
    myQueue.put(10)
    myQueue.get() should be (__)
    myQueue.get() should be (__)
  }

  koan("Just like other traits, stackable traits can be mixed after the fact") {
    trait Doubling extends IntQueue {
      abstract override def put(x: Int) { super.put(2 * x) } //abstract override is necessary to stack traits
    }

    val myQueue = new BasicIntQueue with Doubling //mixin during instantiation

    myQueue.put(40)
    myQueue.get() should be (__)
  }

  koan(
    """More traits can be stacked one atop another, make sure that all overrides
      | are labelled, abstract override.  The order of the mixins are important.
      | Traits on the right take effect first.""") {

    trait Doubling extends IntQueue {
      abstract override def put(x: Int) { super.put(2 * x) } //abstract override is necessary to stack traits
    }

    trait Incrementing extends IntQueue {
      abstract override def put(x: Int) { super.put(x + 1) }
    }

    val myQueue = new BasicIntQueue with Doubling with Incrementing //mixin during instantiation
    myQueue put 4
    myQueue put 3

    myQueue.get should be (__)
    myQueue.get should be (__)
  }


  koan(
    """Same koans as before except that we swapped the order of the traits""") {

    trait Doubling extends IntQueue {
      abstract override def put(x: Int) { super.put(2 * x) } //abstract override is necessary to stack traits
    }

    trait Incrementing extends IntQueue {
      abstract override def put(x: Int) { super.put(x + 1) }
    }

    val myQueue = new BasicIntQueue with Incrementing with Doubling //mixin during instantiation
    myQueue put 4
    myQueue put 3
    myQueue.get should be (__)
    myQueue.get should be (__)
  }


  koan(
    """Using three traits to enhance the IntQueue: Doubling, Incrementing, and Filtering!""") {

    trait Doubling extends IntQueue {
      abstract override def put(x: Int) { super.put(2 * x) } //abstract override is necessary to stack traits
    }

    trait Incrementing extends IntQueue {
      abstract override def put(x: Int) { super.put(x + 1) }
    }

    trait Filtering extends IntQueue {
      abstract override def put(x: Int) {
        if (x >= 0) super.put(x)
      }
    }

    val myQueue = new BasicIntQueue with Incrementing with Doubling with Filtering //mixin during instantiation
    myQueue put 4
    myQueue put -1
    myQueue put 3
    myQueue.get should be (__)
    myQueue.get should be (__)
  }

  koan("Traits are instantiated before a the mixed-in class instantiation") {
    var sb = List[String]()

    trait T1 {
      sb = sb :+ "Instantiated T1"
    }

    class C1 extends T1 {
      sb = sb :+ "Instantiated C1"
    }

    sb = sb :+ "Creating C1"
    new C1
    sb = sb :+ "Created C1"

    sb.mkString(";") should be(__)
  }


  koan("Traits are instantiated before a classes instantiation from left to right") {
    var sb = List[String]()

    trait T1 {
      sb = sb :+ "Instantiated T1"
    }

    trait T2 {
      sb = sb :+ "Instantiated T2"
    }

    class C1 extends T1 with T2 {
      sb = sb :+ "Instantiated C1"
    }

    sb = sb :+ "Creating C1"
    new C1
    sb = sb :+ "Created C1"

    sb.mkString(";") should be(__)
  }

  koan("Instantiations are tracked internally and will not allow a duplicate instantiation. " +
    "Note T1 extends T2, and C1 also extends T2, but T2 is only instantiated once.") {

    var sb = List[String]()

    trait T1 extends T2 {  // Notice: T1 extends T2
      sb = sb :+ "Instantiated T1"
    }

    trait T2 {
      sb = sb :+ "Instantiated T2"
    }

    class C1 extends T1 with T2 {
      sb = sb :+ "Instantiated C1"
    }

    sb = sb :+ "Creating C1"
    new C1
    sb = sb :+ "Created C1"

    sb.mkString(";") should be(__)
  }


  koan("The diamond of death (http://en.wikipedia.org/wiki/Diamond_problem) is avoided since " +
    "instantiations are tracked and will not allow multiple instantiations") {

    var sb = List[String]()

    trait T1 {
      sb = sb :+ "Instantiated T1"
    }

    trait T2 extends T1 {
      sb = sb :+ "Instantiated T2"
    }

    trait T3 extends T1 {
      sb = sb :+ "Instantiated T3"
    }

    class C1 extends T2 with T3 {
      sb = sb :+ "Instantiated C1"
    }

    sb = sb :+ "Creating C1"
    new C1
    sb = sb :+ "Created C1"

    sb.mkString(";") should be(__)
  }
}
