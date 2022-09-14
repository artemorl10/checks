import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object PojoCreator2 extends App {
  val orderredValues = """trade_leg_key,trade_key,source_leg_strip_cd,trade_txn_cd,trade_txn_key,period_start_date,period_end_date,refreshed_dt,commodity_id,buy_sell_ind,legal_agreement_3_key,legal_agreement_4_key,legal_agreement_5_key,gtc_legal_agreement_key,netting_3_flag,netting_4_flag,netting_5_flag,chain_key,csa_ind,derivative_product_cd,credit_rights_amd_ind,pay_price,pay_price_uom_cd,pay_price_currency_cd,pay_start_dt,pay_end_dt,pay_calendar_cd,pay_trade_event_cd,pay_from_after_ind,pay_offset_month_cd,pay_sett_on_day_of_mth_num,pay_offset_from_event,pay_split_pre_next_ind,pay_split_pre_next_adt,pay_cal_work_day_ind,pay_fixed_settlement_dt,pay_fixed_settlement_adt,pay_pricing_term_cd,pay_curve_key,receive_price,receive_currency_cd,receive_uom_cd,receive_start_dt,receive_end_dt,receive_curve_key,receive_pricing_term_cd,contract_quantity_amt,contract_quantity_uom_cd,valuation_quantity_amt,valuation_quantity_uom_cd,mtm_value_amt,mtm_value_currency_cd,settlement_value_amt,settlement_value_currency_cd,payment_due_date,price_settle_frequency_cd,invoice_dt,return_exposure_ind,no_exp_return_reason_cd,src_leg_last_updt_dtm,payment_security_cd,source_valuation_dt,create_dtm,last_updt_dtm,create_user_id,update_user_id,delete_flag,indexing_basis_cd,deal_formula_txt,market_formula_txt,chain_number""".stripMargin
  val ordered = orderredValues.split(",")
  val lines = Source.fromFile("OdsCraveTradeSwapLeg.txt").getLines.toList

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
                  |    val o = new POJO()""".stripMargin)

  types.foreach {t =>
    println(t)
  }


  var counter = 0
  ordered.foreach{ field =>
    val valueType = types(field)
    valueType match {
      case "String" => output.append(s"o.set${field.capitalize}" +
        s"(this.${field.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")})\n")
        counter = counter + 1
      case "Date" => output.append(s"""o.set${field.capitalize}(Date.valueOf(
                                      |      ZonedDateTime.parse(this.${field
        .split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")}, DateTimeFormatter.ISO_OFFSET_DATE_TIME).toLocalDate))\n""".stripMargin)
        counter = counter + 1
      case "Long" =>
        val scalaValue = field.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(
        s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
           |  o.set${field.capitalize}(null)
           |else o.set${field.capitalize}(this.$scalaValue.toLong)\n""".stripMargin)
        counter = counter + 1
      case "Double" =>
        val scalaValue = field.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(
        s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
           |  o.set${field.capitalize}(null)
           |else o.set${field.capitalize}(this.$scalaValue.toDouble)\n""".stripMargin)
        counter = counter + 1
      case "Boolean" =>
        val scalaValue = field.split("(?=\\p{Upper})").map(_.toLowerCase).mkString("_")
        output.append(
        s"""if (this.$scalaValue == null || this.$scalaValue.equals("null"))
           |  o.set${field.capitalize}(null)
           |else o.set${field.capitalize}(this.$scalaValue == "Y")\n""".stripMargin)
        counter = counter + 1
      case _ =>
    }
  }
  println(output.toString())
  println(s"Types size: ${types.size}")
  println(s"Apended: ${counter}")
//  println(types)
  println(ordered.length)
}
