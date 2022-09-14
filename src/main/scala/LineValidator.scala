import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object LineValidator extends App {
  val lines = Source.fromFile("inputc.txt").getLines.toList

  val output = new StringBuilder
  val order: ListBuffer[String] = ListBuffer.empty[String]
  val types: mutable.LinkedHashMap[String, Int] = mutable.LinkedHashMap.empty[String, Int]
  var open = false
  lines.foreach { line =>
    line match {
      case l1 if l1.startsWith("case class") =>
        open = true
      case l2 if l2.contains("val ") && open => {
        println(l2)
        val vars: String = l2
          .replace("val ", "")
          .trim
          .split(": ")(0)
        types += (vars -> 0)
      }
      case l3 if l3.trim.startsWith(")") && open => {
        open = false
      }
      case l5 if l5.contains("_") && !open =>
        println(s"here -> $l5")
        val key = l5
          .replace(")", "")
          .split(" ")
          .flatMap(el => el.split("\\."))
          .find(_.contains("_"))
        println(s"here key -> ${key.mkString(", ")}")
        if (key.isDefined && types.get(key.get).isDefined)
          types += (key.get -> (types(key.get) + 1))
      case _ =>
    }
  }
  types.foreach(t => println(s"${t._1} -> ${t._2}"))
}
