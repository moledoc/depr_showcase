import scala.io.Source
import scala.collection.mutable.ListBuffer

object Expenses{

    class Expense(d:String,e:Double,sec:String,desc:String){
        var date = d
        var expense = e
        var section = sec
        var description = desc

        // def toString(exp: Expense): String = {
        //     exp.date ++ exp.expense.toString ++ exp.section ++ exp.description
        // }
    }

    def makeExpense(line: String): Expense = {
        val elements = line.split(",")
        new Expense(elements(0),elements(1).toDouble,elements(2),elements(3))
    }
    
    def expenseSum(exp: ListBuffer[Expense]): Double = {
        exp.foldLeft(0.0)((acc,x)=>acc+x.expense)
    }

    def makeReport(exp: ListBuffer[Expense]): Unit = {
        // get user input for start and end date
        val start = scala.io.StdIn.readLine("Start date: ")
        val end = scala.io.StdIn.readLine("End date: ")
        val expensesList = exp.filter(x => x.date>=start && x.date<end)

        // calculate total
        println("TOTAL: " ++ expenseSum(expensesList).toString)

        // calculate by section
        expensesList.groupBy(_.section).foreach(x=>println(" -- " ++ x._1 ++ ": " ++ expenseSum(x._2).toString))
    }

    def main(args: Array[String]): Unit = {
        var expensesList = new ListBuffer[Expense]()

        // read in file and make expenses
        val bufferedSource = Source.fromFile("data.csv")
        for (line <- bufferedSource.getLines) {
            if (!line.toUpperCase.contains("EXPENSE")){
                expensesList += makeExpense(line.toUpperCase)
            }
        }
        bufferedSource.close

        // // Report
        // makeReport(expensesList)
        
    }
}
