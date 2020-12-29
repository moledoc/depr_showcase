import scala.io.Source
import scala.collection.immutable.ListMap
import scalafx.application.JFXApp
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button, DatePicker, Tab, TabPane, TextArea, TextField}
import scalafx.scene.layout.{BorderPane, VBox}

import java.io._
import java.util.Calendar

object Expenses extends JFXApp {
  val datafile = "data.csv"
  val buttonWidth = 125
  var header = ""

  class Expense(d: String, e: Double, sec: String, desc: String) {
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
    new Expense(elements(0), elements(1).toDouble, elements(2), elements(3))
  }

  def expenseSum(exp: List[Expense]): Double = {
    exp.foldLeft(0.0)((acc, x) => acc + x.expense)
  }

  def makeReport(exp: List[Expense], start: String, end: String): String = {
    //    // get user input for start and end date
    //    val start = scala.io.StdIn.readLine("Start date: ")
    //    val end = scala.io.StdIn.readLine("End date: ")
    val expensesList = exp.filter(x => x.date >= start && x.date < end)
    // calculate report
    val output = expensesList.groupBy(_.section).foldRight(start ++ " -- " ++ end ++ "\n" ++ "TOTAL: " ++ expenseSum(expensesList).toString)((x, acc) => acc ++ "\n -- " ++ x._1 ++ ": " ++ expenseSum(x._2).toString)
    output
  }

  def addExpense(exp: Map[Int, Expense], date: String, expense: Double, section: String, description: String): Map[Int, Expense] = {
    //    val date = scala.io.StdIn.readLine("Date: ")
    //    val expense = scala.io.StdIn.readLine("Amount: ").toDouble
    //    val section = scala.io.StdIn.readLine("Type: ")
    //    val description = scala.io.StdIn.readLine("Description: ")
    val newExp = new Expense(date, expense, section, description)

    val fw = new FileWriter(datafile, true)
    try {
      fw.write(newExp.toString ++ "\n")
    }
    finally fw.close()

    var addNew = exp
    addNew += (exp.keys.max + 1 -> newExp)
    addNew
  }

  def rmExpense(exp: Map[Int, Expense], rmIdx: Int): Map[Int, Expense] = {
    var rmExp = ListMap(exp.toSeq.sortBy(_._1): _*)
    // TODO: printida
    //    val rmIdx = scala.io.StdIn.readLine("Select the index of expense that you want to delete: ").toInt
    rmExp -= rmIdx

    val pw = new PrintWriter(datafile)
    pw.write(header)
    pw.close()
    val fw = new FileWriter(datafile, true)
    try {
      rmExp.foreach(x => fw.write(x._2.toString ++ "\n"))
    }
    finally fw.close()

    rmExp
  }

  def printExpenses(exp: Map[Int, Expense]): String = {
    val outExp = ListMap(exp.toSeq.sortBy(_._1): _*)
    val output = outExp.foldRight(header)((x, acc) => acc ++ x._1.toString ++ ": " ++ x._2.toString ++ "\n")
    output
  }

  //  def printExpenses(exp: Map[Int,Expense]): Map[Int,Expense] = {
  //    var outExp = ListMap(exp.toSeq.sortBy(_._1):_*)
  //    outExp.foreach(x=>println(x._1.toString++": "++x._2.toString))
  //    outExp
  //  }

  def readExpense(): Map[Int, Expense] = {
    var expensesMap = Map[Int, Expense]()

    // read in file and make expenses
    val bufferedSource = Source.fromFile(datafile)
    var i = 1
    for (line <- bufferedSource.getLines) {
      if (!line.toUpperCase.contains("EXPENSE")) {
        expensesMap += (i -> makeExpense(line.toUpperCase))
        i += 1
      } else {
        header = line.toUpperCase++"\n"
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
  //        val bufferedSource = Source.fromFile("data.csv") //
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
      val year = cal.get(Calendar.YEAR)
      val month = cal.get(Calendar.MONTH)+1
      val curMonth = year.toString ++ "-" ++ month.toString ++ "-01"
      val nextMonth = if(month == 12) (year+1).toString ++ "-01-01" else year.toString ++ "-" ++ (month+1).toString ++ "-01"

      // Reporting expenses
      val reportOutput = new TextArea {
        editable = false
        text = makeReport(expensesMap.values.toList, curMonth, nextMonth)
      }

      val startDate = new DatePicker {
        promptText = "Start date"
        maxWidth = buttonWidth
        editable = false
      }
      val endDate = new DatePicker {
        promptText = "End date"
        maxWidth = buttonWidth
        editable = false
      }

      val report = new Button {
        text = "Report"
        onAction = _ => {
          try{
            reportOutput.text = makeReport(expensesMap.values.toList, startDate.value.value.toString, endDate.value.value.toString)
          } catch {
            case e: NullPointerException => new Alert(AlertType.Information) {
              title = "Incorrect input"
              headerText = "Inputs can't be empty"
            }.showAndWait()
          }
          startDate.value = null
          endDate.value = null
//          startDate.promptText = "Start date"
//          endDate.promptText = "End date"
        }
      }

      val reportPane = new BorderPane {
        center = reportOutput
        right = new VBox(startDate, endDate, report)
      }

      val reportTab = new Tab {
        text = "Report"
        content = reportPane
        closable = false
      }

      // Adding expense
      val addOutput = new TextArea {
        editable = false
        text = printExpenses(expensesMap)
      }

      val date = new DatePicker {
        maxWidth = buttonWidth
        promptText = "Date"
        editable = false
      }
      val expense = new TextField {
        maxWidth = buttonWidth
        promptText = "Amount"
      }
      val section = new TextField {
        maxWidth = buttonWidth
        promptText = "Type"
      }
      val description = new TextField {
        maxWidth = buttonWidth
        promptText = "Description"
      }

      val add: Node = new Button {
        text = "Add"
        onAction = _ => {
          try{
            expensesMap = addExpense(expensesMap, date.value.value.toString, expense.text.value.toDouble, section.text.value, description.text.value)
          } catch{
            case e: NumberFormatException => new Alert(AlertType.Information) {
              title = "Incorrect input"
              headerText = "Amount needs to be a number!"
            }.showAndWait()
            case e:NullPointerException => new Alert(AlertType.Information) {
              title = "Incorrect input"
              headerText = "Inputs can't be empty"
            }.showAndWait()
          }
          addOutput.text = printExpenses(expensesMap)
//          addTab.content = addOutput
          date.value = null
          expense.text = null
          section.text = null
          description.text = null
//          date.promptText = "Date"
//          expense.promptText = "Amount"
//          section.promptText = "Type"
//          description.promptText = "Description"
        }
      }

      val addRefresh: Node = new Button {
        text = "Refresh"
        onAction = _ => {
          addOutput.text = printExpenses(expensesMap)
        }
      }

      val addPane = new BorderPane {
        center = addOutput
        right = new VBox(date, expense, section, description, add,addRefresh)
      }

      val addTab = new Tab {
        text = "Add"
        content = addPane
        closable = false
      }

      // Removing expense
      val rmOutput = new TextArea {
        editable = false
        text = printExpenses(expensesMap)
      }

      val rmIdx = new TextField {
        maxWidth = buttonWidth
        promptText = "ID(s)"
      }

      val rm: Node = new Button {
        text = "Remove"
        onAction = _ => {
//          rmIdx.text.value.split(",").foreach(x=>expensesMap = rmExpense(expensesMap, x.toInt))
//          for(x <- rmIdx.text.value.split(",|, |;|.| ")){
          try{
            val idList = rmIdx.text.value.split(", |,|; |;| ")
            val checkedList = idList.map(_.toInt)
            for(x <- checkedList) {
              expensesMap = rmExpense(expensesMap, x)
            }
          } catch{
            case e: NumberFormatException => new Alert(AlertType.Information) {
              title = "Incorrect input"
              headerText = "ID(s) needs to be a number or comma (,) separated list!"
            }.showAndWait()
          }
          rmOutput.text = printExpenses(expensesMap)
          rmIdx.text = null
//          rmIdx.promptText = "ID of expense"
        }
      }

      val rmRefresh: Node = new Button {
        text = "Refresh"
        onAction = _ => {
          rmOutput.text = printExpenses(expensesMap)
        }
      }

      val rmPane = new BorderPane {
        center = rmOutput
        right = new VBox(rmIdx, rm,rmRefresh)
      }

      val rmTab = new Tab {
        text = "Remove"
        content = rmPane
        closable = false
      }

      tabPane.tabs = List(reportTab, addTab, rmTab)
      root = tabPane
    }
  }
}
