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
      val maybeNum: Option[Int] = Some(123)

      val a = if (true) { new C2 } else { new C1 }
      println(a)
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
