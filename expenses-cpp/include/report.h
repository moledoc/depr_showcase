//! \file report.h
#pragma once

#include <iostream>
#include <unordered_map>
#include <cmath>
#include <string>
#include <iomanip>


//TODO: add other functionalities.
class Report{

public:
	//! Filename where the expense is defined in.
	std::string section;
	//! Flag that was given by the user.
	std::string flag;
	//! Period when the expenses are summarized. Format yyyy-mm-dd -- yyyy-mm-dd
	std::string date_period;
	//! Total value of the expenses in the given period.
	float total_expenses = 0;
	//! Value of the expenses grouped by description in the given period.
	std::unordered_map<std::string, float> expenses_by_desc;
	
	//! Default constructor.
	Report(){}
	
	/*!
 	 * \brief Make report into presentable form.
 	 *
 	 */
	std::string toString(){
		std::stringstream ss;
		ss << std::setprecision(6);
		ss << section << "\nFlag: " <<flag <<"\nPeriod: " <<date_period
				<<"\nThe total amount: "
				<<total_expenses <<"\n";

		// Add sum by description
		for (auto& it:expenses_by_desc){
			ss <<it.first<<": "<<it.second<<"\n";
		};
		std::string result = ss.str();

		return result;
	}
	
	//! Overwrite << operator for easily showing formated report.
	friend std::ostream& operator <<(std::ostream& stream, Report& r){
		stream << r.toString();
		return stream;
	};
};

/*! 
 * \brief Help function, to have correct rounding capabilities.
 * \return The rounded value.
 */
float round(float x) { return floor(x * 100 + 0.5) / 100; }

/*!
 * \brief Function to calculate total sum of expenses.
 * \return The sum of all expeneses in vector<Expense>.
 */
float summed_total(std::vector<Expense> expenses){
	float sum = 0;
	for (Expense expense : expenses){
		sum += expense.expense;
	};
	return round(sum);
};

/*!
 * \brief Function to calculate sum of expenses for each description.
 * \return Unordered map of descriptions and corresponding sums of expenses.
 */
std::unordered_map<std::string, float> summed_by_desc(std::vector<Expense> expenses){
	std::unordered_map<std::string, float> grouped;
	for (Expense expense : expenses){
		if(grouped.find(expense.description) == grouped.end()){
			grouped.insert({expense.description,expense.expense});
		} else {
			grouped.at(expense.description) += expense.expense;
		};
	};
	return grouped;
};

