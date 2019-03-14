package com.example

import unfiltered.request._
import unfiltered.response._
import unfiltered.directives._
import Directives._

import scala.collection.mutable

class Printer {
  def echo(str: String) : Unit = println(str)
}

trait A {}
trait B {}
trait C

class C1 extends C with B with A {}
class C2 extends A with B {}

//** unfiltered plan */
object ExampleApp {

  def double(n: Int) : Int = n * 2

  def multi(n: Int, m: Int) : Int = n * m

  def intent = unfiltered.filter.Planify {
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
    unfiltered.jetty.Server.anylocal.context("/assets") {
      _.resources(new java.net.URL(getClass().getResource("/www/css"), "."))
    }.plan(ExampleApp.intent).run({ svr =>
      unfiltered.util.Browser.open(svr.portBindings.head.url + "/a/b")
    })
  }
}
