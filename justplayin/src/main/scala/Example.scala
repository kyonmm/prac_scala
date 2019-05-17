package com.example

import java.io.File

import unfiltered.request._
import unfiltered.response._
import unfiltered.directives._
import Directives._

import scala.collection.mutable
import scala.util.Try

class Printer {
  def echo(str: String) : Unit = println(str)
}

trait A {}
trait B {}
trait C

class C1 extends C with B with A {}
class C2 extends A with B {}
class Printer2(x: Int){
  /* private */ def print(): Unit = println(x)
}

abstract class Super{
//  final def foo: Unit = println("Foo")
  def foo: Unit
  protected def puts(message: String): Unit = {
    println(message)
  }
}
class Sub extends Super {
  override final def foo: Unit = println("Sub")

  def sub():Unit  = {
    puts("sub()")
    //m.puts("sub()")
  }
}
class User {
//  val m = new Super
  // error
  //m.puts("User")
}
class Cell[A](var value: A){
  def put(newValue: A): Unit ={
    value = newValue
  }
  def get: A = value
}

class Circle(x:Int, y:Int, radius: Int ){
  lazy val area :Double = {
   println("Circle")
   radius * radius *math.Pi
 }

}

//** unfiltered plan */
object ExampleApp {

  def double(n: Int) : Int = n * 2

  def multi(n: Int, m: Int) : Int = n * m

  def factorial(n: Int): Int = {
    def loop(m: Int, x: Int): Int = if (m == 0) {
      x
    } else {
      loop(m - 1, x * m)
    }
    loop(n, 1)
  }

  implicit def intToBoolean(n: Int): Boolean = n != 0

  class RichInt(val self: Int) {
    def isPositive: Boolean = self > 0
  }

  implicit def enrichInt(self: Int): RichInt = new RichInt(self)

  implicit class RichInt2(val self: Int) {
    def isPositive2: Boolean = self > 0
  }


  def intent = unfiltered.filter.Planify {
    case Path(Seg("2-7"::rest)) =>
      rest match {
        case List("local-method", n) =>
          val num = n.toInt
          ResponseString(factorial(num).toString())
        case other =>
          ResponseString(other.mkString("/"))
      }
    case Path(Seg("2-8"::rest)) =>
      rest match {
        case List("access-default") =>
          (new Printer2(1)).print()
          ResponseString("")

        case List("lazy") =>
          val c = new Circle(0,0,5)
          println("-------")
          c.area
          ResponseString(c.area.toString())

        case other =>
          ResponseString(other.mkString("/"))
      }
    case Path(Seg("2-9"::rest)) =>
      rest match {
        case List("generics") =>
          val cell = new Cell[String]("Hello")
          println(cell.get)
          cell.put("World")
          ResponseString(cell.get)


        case other =>
          ResponseString(other.mkString("/"))
      }
    case Path(Seg("2-11"::rest)) =>
      rest match {
        case List("anon-class") =>
          new Thread {
            override def run(): Unit = {
              for (i <- 1 to 10) println(i)
            }
          }.start()
          ResponseString("start thread...")
        case other =>
          ResponseString(other.mkString("/"))
      }
    case Path(Seg("2-12"::rest)) =>
      rest match {
        case List("implicit-conversion", n) =>
          if (n.toInt) {
            ResponseString("true")
          } else {
            ResponseString("false")
          }
        case List("extends-method", n) =>
          ResponseString(n.toInt.isPositive.toString())
        case other =>
          ResponseString(other.mkString("/"))
      }
    case Path(Seg("2-14"::rest)) =>
      rest match {
        case List("implicit-parameter1", n) =>
          implicit val context = n.toInt
          def printContext(implicit ctx: Int): Unit = {
            println(ctx)
          }

          printContext
          ResponseString("")

        case List("implicit-parameter2", n) =>
          def printContext(implicit ctx: Int): Unit = {
            println(ctx)
          }
          def printContext2(implicit ctx: Int): Unit = {
            printContext
          }

          implicit val context = n.toInt * 2
          printContext2
          ResponseString("")

        case List("implicit-parameter3") =>
          def  sumInt(list: List[Int]): Int = list.foldLeft(0){
            (x, y)=> x + y;
          }

          trait Adder[T]{
            def zero: T
            def plus(x: T, y: T): T
          }

          def sum[T](list: List[T])(adder: Adder[T]): T = {
            list.foldLeft(adder.zero){(x, y) => adder.plus(x, y)
            }
          }

          object IntAdder extends Adder[Int] {
            def zero: Int = 0
            def plus(x: Int, y: Int): Int = x + y
          }

          ResponseString(sum(List(1, 2, 3))(IntAdder).toString())

        case "implicit-parameter4"::rest =>
          trait Adder[T]{
            def zero: T
            def plus(x: T, y: T): T
          }

          def sum[T](list: List[T])(implicit adder: Adder[T]): T = {
            list.foldLeft(adder.zero){(x, y) => adder.plus(x, y)
            }
          }

          implicit object IntAdder extends Adder[Int] {
            def zero: Int = 0
            def plus(x: Int, y: Int): Int = x + y
          }

          implicit object StringAdder extends Adder[String] {
            def zero: String = ""
            def plus(x: String, y: String): String = x + y
          }

          ResponseString(sum(rest).toString())
        case other =>
          ResponseString(other.mkString("/"))
      }
    case Path(Seg("3-1"::rest)) =>
      rest match {
        case List("Option1") =>
          val directory = new File("存在しないディレクトリ")
          def myListFiles(directory: File): Option[Array[File]] =
            Option(directory.listFiles())
          val maybeFiles = myListFiles(directory)

          ResponseString("")

        case List("Option2") =>
          def fileSize(file: File): Option[Long] =
            if (file.exists()) Option(file.length()) else None

          val maybeFile = Option(new File("abc.txt"))

          maybeFile
            .flatMap(fileSize)
            .map(_.toString)
            .foreach(println)

          def plus(option1: Option[Int], option2: Option[Int]): Option[Int] =
            option1.flatMap(i => option2.map(j => i + j))

          plus(Option(2), Option(3))
              .foreach(println)

          plus(Option(2), None)
              .foreach(println)

          plus(None, Option(2))
            .foreach(println)

          plus(None, None)
            .foreach(println)

          ResponseString("done.")

        case List("Option3") =>
          def plus(option1: Option[Int], option2: Option[Int]): Option[Int] =
            for(i <- option1; j<-option2)yield i+j


          plus(Option(2), Option(3))
            .foreach(println)

          plus(Option(2), None)
            .foreach(println)

          plus(None, Option(2))
            .foreach(println)

          plus(None, None)
            .foreach(println)

          def getIntOrZero(option: Option[Int]):Int = option.getOrElse(0)
          println(getIntOrZero(Option(123)))
          println(getIntOrZero(None))



          ResponseString("done.")

        case List("Option4") =>
          def plus(option1: Option[Int], option2: Option[Int]): Option[Int] =
            for(i <- option1; j<-option2)yield i+j
          plus(Option(2), Option(3))
            .foreach(println)

          plus(Option(2), Option(3)) match{
            case Some(v) => println(v)
            case None =>
          }

          plus(Option(2), None)
            .foreach(println)

          plus(None, Option(2))
            .foreach(println)

          plus(None, None)
            .foreach(println)

          def getIntOrZero(option: Option[Int]):Int = option match{
            case Some(v) => v
            case None => 0
          }

          println(getIntOrZero(Option(123)))
          println(getIntOrZero(None))

          ResponseString("done.")


      }
    case Path(Seg("3-2"::rest)) =>
      rest match {
        case List("FileSize1") =>
          def fileSize(file: File): Either[String, Long] =
          if(file.exists()) Right(file.length()) else Left("File not exists")
        ResponseString(fileSize(new File("")).toString)

        case List("FileSize2") =>
          val r:Either[String, Int] = Right(100)
          r.foreach(println)
          r.left.foreach(println)
          val l:Either[String, Int] = Left("Hello")
          l.foreach(println)
          l.left.foreach(println)
          ResponseString("")

        case List("FileSize3") =>
          val r:Either[String, Long] = Right(1)
          println(r.map(_ * 2).toString)
          println(r.right.map(_ * 2).toString)
          println(r.left.map(_ + "!").toString)
          ResponseString("")

        case List("FileSize4") =>
          val r:Either[String, Long] = Right(100)
          println(r.flatMap(l => Right(l * 5)).toString)
          println(r.flatMap(_ => Left("Error")).toString)
          val l1: Either[String, Long] = Left("Error 1")
          println(l1.flatMap(l => Right(l * 5)).toString)
          ResponseString("")

        case List("FileSize5") =>
          val value1 = Right(1).getOrElse(100)
          println(value1)
          val value2 = Left("foo").getOrElse(100)
          println(value2)
          ResponseString("")

        case List("FileSize6") =>
          val intEither = Right(123)
          println(intEither.merge)
          val stringEither : Either[String, String] = Left("foo")
          println(stringEither.merge)
          ResponseString("")
      }
    case Path(Seg("3-3"::rest)) =>
      rest match {
        case List("Try") =>
          def div(a: Int,b: Int): Try[Int] = Try(a/b)
          println(div(10,3).toString())
          println(div(10,0).toString())
          println(div(10,0).recover{case e: ArithmeticException => 0})
          println(div(10,0).recoverWith{case e: ArithmeticException => Try(1 + 1)})
          ResponseString("")
      }



        case Path(Seg(p :: p2 :: Nil)) =>
      val name = "Scala"
//      name = "Java"
      val printer = new Printer
      printer.echo(name)
      val d = double _
      val m = multi _
      val weight = 120
      val message = if(weight <= 100){
        "OK"
      } else{
        "Over"
      }
      p match {
        case "a" => println("A")
        case "b" => println("B")
        case other => println("prize," + other)
      }
      val maybeNum: Option[Int]=Some(-10)
      val num:Int=maybeNum match{
        //case Some(num) if num < 0 => 0
        case Some(num) => if (num < 0) 0 else num
        case None => 0
      }
      num match{
        case 1|2|3 =>println("Less than 4")
        case 4 => println("Equal to 4")
        case other => println("Greater than 4")
      }

      var i = 0
      while(i < 3) {
        println(i)
        i += 1
      }

      val filtered = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13).filter(_ > 1)
      println(filtered)

      val doubled = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) map (i => i * 2)
      println(doubled)

      println(Seq(Seq(1, 2), Seq(3, 4)).flatMap{s => println(s); s})

      val results: Seq[Int] = (1 to 3).flatMap{ i =>
        (2 to 4).flatMap { j =>
          (3 to 5).map( k => i * j * k).filter(_ % 3 == 0)
        }
      }

      val results2: Seq[Int] = for {
        i <- 1 to 3
        j <- 2 to 4
        k <- 3 to 5
        result = i * j * k if result % 3 == 0
      } yield result

      println(results)
      println(results2)

      def puts(value: String): Unit = {
        println(value)
      }

      puts("Hello, World!")

      val x: Unit = ()
      val y: Unit = x
      //println(x)
      //println(y)

      val z: String = null
      //println(z)

//      def add(x: Int, y: Int): Int = ???
//      println(add(1,2))

//      def requirePositive(n: Int): Int = if (n > 0) n else throw new IllegalArgumentException("n must be positive")

      val a = """ |
         | This is
         | Tamago
         | Hay!""".stripMargin

      """
        |aaa
        |bbb
        |ccc
      """.stripMargin

      val b =s"1+2=${1+2}"
      println(b)

      val pp: (Int, Int) = (10, 20)
      println(pp._1)
      println(pp._2)

      val ppp:(Int, Int, Int) = (30, 40, 50)
      ppp match {
        case (x, 10, _) => x
        case (10, y, _) => y
        case (x, y, z) =>
          println(x)
          println(y)
          println(z)
      }
      //val (x, y, z) = ppp

      type Point = (Int, Int, Int)
      val po: Point = (60, 70, 80)
      println(po.getClass)

      class PointC(vx: Int, vy: Int) {
        var x = vx
        var y = vy
        var sumNumber: Int = _

        println(x)
        def distance(that: PointC): Int = {
          val xdiff = math.abs(this.x - that.x)
          val ydiff = math.abs(this.y - that.y)
          math.sqrt(xdiff * xdiff + ydiff * ydiff).toInt
        }
        def +(that: PointC): PointC =
          new PointC(this.x + that.x, this.y + that.y)

        println(y)
      }

      var point = new PointC(11, 12)
      println(point.x)
      println(point.y)
      println(point.sumNumber)


      abstract class Shape {
        def draw(): Unit = {
          println("不明な図形")
        }
      }

      class Triangle extends Shape {
        override def draw(): Unit = {
          println("三角形")
        }
      }

      class Rectangle extends Shape {
        override def draw(): Unit = {
          println ("四角形")
        }
      }

      class UnknownShape extends Shape

      var shape: Shape = new Triangle
      shape.draw()
      shape = new Rectangle
      shape.draw()
      shape = new UnknownShape
      shape.draw()

      trait Namable {
        val name: String
        def display(): Unit = println(name)
      }

      class Employee(val name:String) extends AnyRef with Namable

      val taro = new Employee("太郎")
      taro.display()

      trait Enumerable[A] {
        def foreach[B](fun: A => B):Unit

        final def map[B](f:A => B):List[B] = {
          var members = mutable.Buffer.empty[B]
          foreach { m =>
            members += f(m)
          }
          members.toList
        }

        final def filter(p:A => Boolean):List[A] = {
          var members = mutable.Buffer.empty[A]
          foreach { m =>
            if(p(m)) members += m
          }
          members.toList
        }

        final def toList: List[A] = {
          var members = mutable.Buffer.empty[A]
          foreach { m =>
            members += m
          }
          members.toList
        }
      }

      case class Staff(val name: String, val age: Int)

      class Shop(val name: String) extends AnyRef with Enumerable[Staff]with Namable {
        private[this] val staffs: List[Staff] = List(new Staff("太郎", 18),
          new Staff("花子", 20))

        override def foreach[B](f:Staff => B):Unit = staffs.foreach(f)
      }

      val shop = new Shop("great shop")
      println(shop.filter(_.age >= 20))
      println(shop.map(_.name))
      println(shop.toList)
      shop.display()

      object Foo extends Namable {
        def foo(): Unit ={
          println("foo")
        }
        val name = "aaa"
      }

      val namable : Namable = Foo
      def displayName(namable: Namable) = namable.display()
      displayName(shop)
      displayName(Foo)
      namable.display()

      Foo.foo()

      object Add{
        def apply(x:Int,y:Int):Int=x+y
      }

      println(Add.apply(1,2))
      println(Add(1,2))


      class MyString (val content :String){
        def unary_! : String = "!"+content
      }
      val s = new MyString("Taro")
      println(!s)

      case class CPoint(x: Int, y: Int)
      val map = Map(CPoint(10, 10) -> 1, CPoint(20, 20) -> 2)
      println(map(CPoint(10, 10)))
      println(map(CPoint(20, 20)))

      val cp = CPoint(3, 4)
      cp match {
        case CPoint(x, y) =>
          println(x)
          println(y)
      }

      val v = cp match {
        case CPoint(3, _) => "OK"
        case _ => "NOOOO!!!!"
      }

      println(v)

      val ab = if(3 < 4) "a" else "b"
      println(ab)

      val abc = { println("hello");1+1}
      println(abc)

      def foo(): String = {
        "foo" + "foo"
      }

      println(foo())

      var ii = 1
      val w = while ( ii <= 10){
        println("ii = " + ii)
        ii = ii + 1
      }

      println(w)

      for(x <- 1 to 3; y <- 1 until 3 if x != y){
        println("x =" + x + " y = " + y)
      }

      for( e <- List(1,2,3)) println(e)

      println(for( e <- List(1,2,3)) yield {
        e + 1
      } )

      val iii = 5
      println(iii match {
        case 0 => "A"
        case _ => "VVV"
      })

      val list= List(1,2,3,4,5,6)
      list match {
        case List(a,b,c,d,e) =>
        println(a,b,c,d,e)
        case _ =>
          println("?")
      }

      def reverse[A](list: List[A],result:List[A]):List[A] = list match {
        case x::xs => reverse(xs,x::result)
        case Nil => result
      }

      println(reverse(List(1,2,3), List(5)))

      val lst = List("A","B","C","D","E")
      lst match {
        case List("A",b,c,d,e) if b != "B" =>
          println("b=" + b)
        case _ =>
          println("nothing")
      }

      def last2[A](list: List[A]): A = list match {
        case x::_::Nil => x
        case x::xs => last2(xs)
        case _ => sys.error("???")
      }
      println(last2(List(1,2,3,4,5)))

      try {
        throw new RuntimeException("Ex")
      } catch {
        case e: RuntimeException => println("GGG")
        case e: Exception => println(e.getMessage)
      }


      def factorial(n: Int): Int = {
        def f(m: Int, x: Int): Int = if (m == 0) {
          x
        } else {
          f(m - 1, x * m)
        }
        f(n, 1)
      }

      println(factorial(10))

      class Printer2(x: Int) {
        def print(): Unit = println(x)
      }
      new Printer2(1).print()

      class Super {
        final def foo: Unit = println("Foo")

        protected def puts(message: String): Unit = {
          println(message)
        }
      }

      class Sub extends Super {
        //override final def foo: Unit = println("Sub")

        def sub(): Unit = {
          puts("sub()")
        }
      }
      new Sub().sub()
      class User {
        val m = new Super
//        m.puts("user")
      }

      class Circle(x: Int, y: Int, radius: Int){
        lazy val area: Double = {
          println("面積を計算します")
          radius * radius * math.Pi
        }
      }

      val c = new Circle(0, 0, 5)
      println("ざっきー")
      println(c.area)
      println(c.area)


      abstract class Super2{
        def foo:Unit
      }

      new Super2 {
        override def foo: Unit = println("super 2")
      }.foo

      class Cell[A](var value: A) {
        def put(newValue: A): Unit = {
          value = newValue
        }
        def get: A = value
      }

      val cell = new Cell[String]("Hallo")
      println(cell.get)
      cell.put("World")
      println(cell.get)


      import com.example.mypackage._

      hallo()

//      new Thread {
//        override def run(): Unit = {
//          for(i <- 1 to 10) println(i)
//        }
//      }.start()

      if (1) {
        println("1 is true")
      }
      implicit def intToBoolean(n: Int): Boolean = n != 0

      class RichInt(val self:Int) {
        def isPositive: Boolean = self > 0
      }
      implicit def enrichInt(self: Int):RichInt = new RichInt(self)
      println(1.isPositive)
      println(0.isPositive)

      implicit class RichInt2(val self: Int) {
        def isPositive2: Boolean = self > 0
      }
      println(1.isPositive2)

      def printContext(implicit ctx: Double): Unit = {
        println(ctx)
      }

      def printContext2(implicit ctx: Double): Unit = {
        printContext
      }
      implicit val context = 2.0
      printContext2

      def sumInt(list: List[Int]): Int = list.foldLeft(0) {
        (x, y) => x + y
      }
      println(sumInt(List(1,2,3,4,5)))

      def sumDouble(list: List[Double]): Double = list.foldLeft(0.0) {
        (x, y) => x + y
      }
      println(sumDouble(List(1,2,3,4,5)))

      def sumString(list: List[String]): String = list.foldLeft("") {
        (x, y) => x + y
      }
      println(sumString(List("a", "b", "c")))

      trait Adder[T] {
        def zero: T
        def plus(x: T, y: T): T
      }
      def sum[T](list: List[T])(implicit adder: Adder[T]): T = {
        list.foldLeft(adder.zero) {
          (x, y) => adder.plus(x, y)
        }
      }
      implicit object IntAdder extends Adder[Int] {
        def zero: Int = 0
        def plus(x: Int, y: Int): Int = x + y
      }
      println(sum(List(1,2,3,4,5)))
      implicit object StringAdder extends Adder[String] {
        def zero: String = ""
        def plus(x: String, y: String): String = x + y
      }
      println(sum(List("a", "b", "c")))

//      val dir = new File("hoge").listFiles()
//      dir.length

      def myListFiles(directory: File): Option[Array[File]] = {
        Option(directory.listFiles())
      }
      val directory = new File("not exist")
      val maybeFiles = myListFiles(directory)
      println(maybeFiles.map(_.length).getOrElse(-1))

      // p.105
      println(Option("hello"))
      println(None)
      println(Option(null))

      def fileSize(file: File): Option[Long] =
        if (file.exists()) Option(file.length()) else None
      println(fileSize(new File("build.sbt")))

      val CPoint(x1, y1) = cp

      val isAlphanumeric = (str: String) => str.matches("[a-zA-Z0-9¥¥s]+")
      ResponseString(p + ", " + p2 + "," + name + d(21) + isAlphanumeric("p") + message)
    case _ => ResponseString(
      "I can echo exactly one path element."
    )
  }
}

/** embedded server */
object Server {
  def main(args: Array[String]): Unit = {
    unfiltered.jetty.Server.local(8081).context("/assets") {
      _.resources(new java.net.URL(getClass().getResource("/www/css"), "."))
    }.plan(ExampleApp.intent).run({ svr =>
      unfiltered.util.Browser.open(svr.portBindings.head.url + "/")
    })
  }
}

