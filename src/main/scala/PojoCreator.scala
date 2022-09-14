import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object PojoCreator extends App {
  val lines = Source.fromFile("input.txt").getLines.toList

  val output = new StringBuilder
  val order: ListBuffer[String] = ListBuffer.empty[String]
  val types: mutable.LinkedHashMap[String, String] = mutable.LinkedHashMap.empty[String, String]
  var open = false
  lines.foreach { line =>
    line match {
      case l1 if l1.startsWith("public class") =>
        open = true
        output.append(s"case class " +
          s"${l1
            .replace("public class", "")
            .replace("}", "")
            .replace("{", "")
            .trim}" +
          s" (\n")
      case l2 if l2.contains("private ") && open => {
        println(l2)
        val vars: Array[String] = l2
          .replace("private ", "")
          .replace(";", "")
          .trim
          .split(" ")
          .map(_.trim)
        types += (vars(1) -> vars(0))
        output.append(s"val ${vars(1).split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")}: String,\n")
      }
      case l3 if l3.trim.startsWith("public") && open => {
        open = false
        output.append(")\n")
      }
      case l5: String if open => println(s"Unhandled line: $l5")
      case _ =>
    }
  }
  output.append("""extends JavaFactory[SwapLegImpl, POJO] {
                  |  import java.time.ZonedDateTime
                  |  import java.time.format.DateTimeFormatter
                  |  private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
                  |
                  |  @Override
                  |  def asJava: POJO = {
                  |    val o = new POJO()
                  |    """.stripMargin)

  types.foreach {t =>
    println(t)
  }


  var counter = 0
  types.foreach{ field =>
    field._2 match {
      case "String" => output.append(s"o.set${field._1.capitalize}" +
        s"(this.${field._1.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")})\n")
        counter = counter + 1
      case "Date" =>
        val scalaValue = field._1.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
//        output.append(s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
//                         |  o.set${field._1.capitalize}(null)
//                         |else o.set${field._1.capitalize}(Date.valueOf(
//                                      |      ZonedDateTime.parse(this.${field._1
//        .split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")}, DateTimeFormatter.ISO_OFFSET_DATE_TIME).toLocalDate))\n""".stripMargin)
        output.append(s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
                         |  o.set${field._1.capitalize}(null)
                         |else o.set${field._1.capitalize}(this.${field._1
        .split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")}.toSqlDate)\n""".stripMargin)
        counter = counter + 1
      case "LocalDate" =>
        val scalaValue = field._1.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
                         |  o.set${field._1.capitalize}(null)
                         |else o.set${field._1.capitalize}(
                                      |      ZonedDateTime.parse(this.${field._1
        .split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")}, DateTimeFormatter.ISO_OFFSET_DATE_TIME).toLocalDate)\n""".stripMargin)
        counter = counter + 1
      case "Long" =>
        val scalaValue = field._1.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(
        s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
           |  o.set${field._1.capitalize}(null)
           |else o.set${field._1.capitalize}(this.$scalaValue.toLong)\n""".stripMargin)
        counter = counter + 1
      case "Integer" =>
        val scalaValue = field._1.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(
        s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
           |  o.set${field._1.capitalize}(null)
           |else o.set${field._1.capitalize}(this.$scalaValue.toInt)\n""".stripMargin)
        counter = counter + 1
      case "Double" =>
        val scalaValue = field._1.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(
        s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
           |  o.set${field._1.capitalize}(null)
           |else o.set${field._1.capitalize}(this.$scalaValue.toDouble)\n""".stripMargin)
        counter = counter + 1
      case "Boolean" =>
        val scalaValue = field._1.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(
        s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
           |  o.set${field._1.capitalize}(null)
           |else o.set${field._1.capitalize}(this.$scalaValue == "Y")\n""".stripMargin)
        counter = counter + 1
      case _ =>
    }
  }
  println(output.toString())
  println(s"Types size: ${types.size}")
  println(s"Apended: ${counter}")
//  println(types)
}
