import scala.collection.mutable
import scala.io.Source

object CsvValidator extends App {

  val orderredValues = """trade_leg_key,trade_key,source_leg_strip_cd,trade_txn_cd,trade_txn_key,period_start_date,period_end_date,refreshed_dt,commodity_id,buy_sell_ind,legal_agreement_3_key,legal_agreement_4_key,legal_agreement_5_key,gtc_legal_agreement_key,netting_3_flag,netting_4_flag,netting_5_flag,chain_key,csa_ind,derivative_product_cd,credit_rights_amd_ind,pay_price,pay_price_uom_cd,pay_price_currency_cd,pay_start_dt,pay_end_dt,pay_calendar_cd,pay_trade_event_cd,pay_from_after_ind,pay_offset_month_cd,pay_sett_on_day_of_mth_num,pay_offset_from_event,pay_split_pre_next_ind,pay_split_pre_next_adt,pay_cal_work_day_ind,pay_fixed_settlement_dt,pay_fixed_settlement_adt,pay_pricing_term_cd,pay_curve_key,receive_price,receive_currency_cd,receive_uom_cd,receive_start_dt,receive_end_dt,receive_curve_key,receive_pricing_term_cd,contract_quantity_amt,contract_quantity_uom_cd,valuation_quantity_amt,valuation_quantity_uom_cd,mtm_value_amt,mtm_value_currency_cd,settlement_value_amt,settlement_value_currency_cd,payment_due_date,price_settle_frequency_cd,invoice_dt,return_exposure_ind,no_exp_return_reason_cd,src_leg_last_updt_dtm,payment_security_cd,source_valuation_dt,create_dtm,last_updt_dtm,create_user_id,update_user_id,delete_flag,indexing_basis_cd,deal_formula_txt,market_formula_txt,chain_number""".stripMargin
  val ordered = orderredValues.split(",")
  val lines = Source.fromFile("ods_crave_trade_swap_leg.csv").getLines.toList
  lines.drop(1).take(1).foreach{ line =>
    val values: List[String] = line.split(",").toList
    val res = ordered.foldLeft((mutable.LinkedHashMap.empty[String, String], 0)) { (mapInd, ind) =>
      (mapInd._1 + (ind -> values(mapInd._2)), mapInd._2 + 1)
    }
    res._1.foreach(pair => println(pair._1 + " -> " + pair._2))
  }

}
