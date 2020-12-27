import scala.io.Source
import java.io._
import scala.collection.immutable.ListMap
import scalafx.application.JFXApp
import scalafx.scene.{Node, Scene}
import scalafx.scene.paint.Color
import scalafx.Includes._
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.geometry.Insets
import scalafx.scene.AccessibleRole.ScrollPane
import scalafx.scene.control.{Button, DateCell, DatePicker, MenuButton, ScrollPane, Slider, Tab, TabPane, TextArea, TextField}
import scalafx.scene.layout.{BorderPane, HBox, TilePane, VBox}

import java.util.Calendar


object Expenses extends JFXApp {
  val datafile = "dat.csv"
  val buttonWidth = 125

  class Expense(d:String,e:Double,sec:String,desc:String){
    var date = d
    var expense = e
    var section = sec
    var description = desc

    override def toString(): String = {
      date ++ "," ++ expense.toString ++ "," ++ section.toUpperCase ++ "," ++ description.toUpperCase
    }
  }

  def makeExpense(line: String): Expense = {
    val elements = line.split(",")
    new Expense(elements(0),elements(1).toDouble,elements(2),elements(3))
  }

  def expenseSum(exp: List[Expense]): Double = {
    exp.foldLeft(0.0)((acc,x)=>acc+x.expense)
  }

  def makeReport(exp:List[Expense],start: String,end: String): String ={
//    // get user input for start and end date
//    val start = scala.io.StdIn.readLine("Start date: ")
//    val end = scala.io.StdIn.readLine("End date: ")
    val expensesList = exp.filter(x => x.date>=start && x.date<end)
    // calculate report
    val output = expensesList.groupBy(_.section).foldRight(start ++ " -- " ++ end ++ "\n" ++ "TOTAL: " ++ expenseSum(expensesList).toString)((x,acc)=>acc ++ "\n -- " ++ x._1 ++ ": " ++ expenseSum(x._2).toString)
    output
  }

  def addExpense(exp:Map[Int,Expense],date:String,expense:Double,section:String,description:String): Map[Int,Expense] = {
//    val date = scala.io.StdIn.readLine("Date: ")
//    val expense = scala.io.StdIn.readLine("Amount: ").toDouble
//    val section = scala.io.StdIn.readLine("Type: ")
//    val description = scala.io.StdIn.readLine("Description: ")
    val newExp = new Expense(date,expense,section,description)

    val fw = new FileWriter(datafile, true)
    try {
      fw.write(newExp.toString++"\n")
    }
    finally fw.close()

    var addNew = exp
    addNew += (exp.keys.max + 1 -> newExp)
    addNew
  }

  def rmExpense(exp:Map[Int,Expense],rmIdx: Int): Map[Int,Expense] = {
    var rmExp = ListMap(exp.toSeq.sortBy(_._1):_*)
    // TODO: printida
//    val rmIdx = scala.io.StdIn.readLine("Select the index of expense that you want to delete: ").toInt
    rmExp-=rmIdx

    val fw = new FileWriter(datafile, true)
    try {
      rmExp.foreach(x=>fw.write(x._2.toString++"\n"))
    }
    finally fw.close()

    rmExp
  }

  def printExpenses(exp: Map[Int,Expense]): String = {
    val outExp = ListMap(exp.toSeq.sortBy(_._1):_*)
    val output = outExp.foldRight("")((x,acc) => acc ++ x._1.toString ++ ": " ++ x._2.toString++"\n")
    output
  }
//  def printExpenses(exp: Map[Int,Expense]): Map[Int,Expense] = {
//    var outExp = ListMap(exp.toSeq.sortBy(_._1):_*)
//    outExp.foreach(x=>println(x._1.toString++": "++x._2.toString))
//    outExp
//  }

  def readExpense(): Map[Int,Expense] = {
    var expensesMap = Map[Int,Expense]()

    // read in file and make expenses
    val bufferedSource = Source.fromFile("data.csv") //TODO: data.csv -> datafile
    var i = 1
    for (line <- bufferedSource.getLines) {
      if (!line.toUpperCase.contains("EXPENSE")){
        expensesMap += (i -> makeExpense(line.toUpperCase))
        i+=1
      }
    }
    bufferedSource.close
    expensesMap
  }

  //    def main(args: Array[String]): Unit = {
  //        var expensesList = new ListBuffer[Expense]()
  //        var expensesMap = Map[Int,Expense]()
  //
  //        // read in file and make expenses
  //        val bufferedSource = Source.fromFile("data.csv") //TODO: data.csv -> datafile
  //        var i = 1
  //        for (line <- bufferedSource.getLines) {
  //            if (!line.toUpperCase.contains("EXPENSE")){
  //                expensesMap += (i -> makeExpense(line.toUpperCase))
  //                i+=1
  //            }
  //        }
  //        bufferedSource.close
  //        // println(expensesMap.values)
  //
  //        // // // Report
  //        // makeReport(expensesMap.values.toList)
  //
  //        // // Add expense
  //        // addExpense(expensesMap)
  //
  //        // // remove expense
  //        //rmExpense(expensesMap)
  //
  //    }
  stage = new JFXApp.PrimaryStage {
    title = "expenses"
    width = 500
    height = 550
    resizable = false
    var expensesMap = readExpense()
    scene = new Scene {
      val tabPane = new TabPane()

      val cal = Calendar.getInstance()
      val curMonth = "2020-01-01" //cal.get(Calendar.DATE).toString
      val nextMonth = "2020-02-01" //cal.get(Calendar.DATE+30).toString

      // Reporting expenses
      val reportOutput = new TextArea {
        editable = false
        text = makeReport(expensesMap.values.toList, curMonth, nextMonth)
      }

      val startDate = new DatePicker{
        promptText = "Start date"
        maxWidth = buttonWidth
        editable = false
      }
      val endDate = new DatePicker{
        promptText = "End date"
        maxWidth = buttonWidth
        editable = false
      }

      val report = new Button {
        text = "Report"
        onAction = _ => {
          reportOutput.text = makeReport(expensesMap.values.toList,startDate.value.toString(),endDate.value.toString())
          startDate.promptText = "Start date"
          endDate.promptText = "End date"
        }
      }

      val reportPane = new BorderPane{
        center = reportOutput
        right = new VBox(startDate,endDate,report)
      }

      val reportTab = new Tab{
        text = "Report"
        content = reportPane
        closable = false
      }

      // Adding expense
      val addOutput = new TextArea{
        editable = false
        text = printExpenses(expensesMap)
      }

      val date = new DatePicker{
        maxWidth = buttonWidth
        promptText = "Date"
        editable = false
      }
      val expense = new TextField{
        maxWidth = buttonWidth
        promptText = "Amount"
      }
      val section = new TextField{
        maxWidth = buttonWidth
        promptText = "Type"
      }
      val description = new TextField{
        maxWidth = buttonWidth
        promptText = "Description"
      }

      val add: Node = new Button {
        text = "Add"
        onAction = _ => {
          expensesMap = addExpense(expensesMap,date.value.toString,expense.text.toString.toDouble,section.text.toString(),description.text.toString)
          addOutput.text = printExpenses(expensesMap)
          addTab.content = addOutput
          date.promptText = "Date"
          expense.promptText = "Amount"
          section.promptText = "Type"
          description.promptText = "Description"
        }
      }

      val addPane = new BorderPane{
        center = addOutput
        right = new VBox(date,expense,section,description,add)
      }

      val addTab = new Tab{
        text = "Add"
        content = addPane
        closable = false
      }

      // Removing expense
      val rmOutput = new TextArea{
        editable = false
        text = printExpenses(expensesMap)
      }

      val rmIdx = new TextField{
        maxWidth = buttonWidth
        promptText = "ID of expense"
      }

      val rm: Node = new Button {
        text = "Remove"
        onAction = _ => {
          expensesMap = rmExpense(expensesMap,rmIdx.toString.toInt)
          rmOutput.text = printExpenses(expensesMap)
          rmTab.content = rmOutput
          rmIdx.promptText = "ID of expense"
        }
      }

      val rmPane = new BorderPane{
        center = rmOutput
        right = new VBox(rmIdx,rm)
      }

      val rmTab = new Tab{
        text = "Remove"
        content = rmPane
        closable = false
      }

      tabPane.tabs = List(reportTab,addTab,rmTab)
      root = tabPane
//      root = new BorderPane {
//        right = new VBox(report,add,rm,print){
//          padding = Insets(10)
//        }
//        top = new HBox(startDate,endDate) // TODO
//        center = output // TODO
//      }
    }
  }

}
