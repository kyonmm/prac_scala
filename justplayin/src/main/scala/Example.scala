package com.example

import unfiltered.request._
import unfiltered.response._

import unfiltered.directives._, Directives._

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

      class PointC(val x: Int, val y: Int) {
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

      new PointC(11, 12)

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
      unfiltered.util.Browser.open(svr.portBindings.head.url)
    })
  }
}
