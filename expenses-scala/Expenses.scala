import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.io._ 
import scala.collection.immutable.ListMap

// import scalafx.application.JFXApp
// import scalafx.scene.Scene
// import scalafx.scene.paint.Color
// import scalafx.Includes._


object Expenses{
    val datafile = "dat.csv"

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

    def makeReport(exp:List[Expense]): Unit ={ 
        // get user input for start and end date
        val start = scala.io.StdIn.readLine("Start date: ")
        val end = scala.io.StdIn.readLine("End date: ")
        val expensesList = exp.filter(x => x.date>=start && x.date<end)

        // calculate total
        println("TOTAL: " ++ expenseSum(expensesList).toString)

        // calculate by section
        expensesList.groupBy(_.section).foreach(x=>println(" -- " ++ x._1 ++ ": " ++ expenseSum(x._2).toString))
    }

    def addExpense(exp:Map[Int,Expense]): Map[Int,Expense] = {
        val date = scala.io.StdIn.readLine("Date: ")
        val expense = scala.io.StdIn.readLine("Amount: ").toDouble
        val section = scala.io.StdIn.readLine("Type: ")
        val description = scala.io.StdIn.readLine("Description: ")
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

    def rmExpense(exp:Map[Int,Expense]): Map[Int,Expense] = {
        var rmExp = ListMap(exp.toSeq.sortBy(_._1):_*)
        rmExp.foreach(x=>println(x._1.toString++": "++x._2.toString))
        val rmIdx = scala.io.StdIn.readLine("Select the index of expense that you want to delete: ").toInt
        rmExp-=rmIdx

        val fw = new FileWriter(datafile, true)
        try {
            rmExp.foreach(x=>fw.write(x._2.toString++"\n"))
        }
        finally fw.close() 

        rmExp
    }

    def main(args: Array[String]): Unit = {
        var expensesList = new ListBuffer[Expense]()
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
        // println(expensesMap.values)

        // // // Report
        // makeReport(expensesMap.values.toList)

        // // Add expense
        // addExpense(expensesMap)

        // // remove expense
        //rmExpense(expensesMap)
        
    }
//   stage = new JFXApp.PrimaryStage {
//       title = "expenses"
//       width = 400
//       height = 300
//       scene = new Scene{
//           fill = Color.LightBlue
//       }
//   }

}
