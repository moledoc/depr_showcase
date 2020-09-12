//! \file expense.h
#pragma once

#include <iostream>
#include <string>

class Expense {

public:
	
	//! Date of the expense.
	std::string date;
	//! Value of the expense.
	float expense;
	//! Description of the expense.
	std::string description;
	//! Filename where the expense is defined in.
	std::string section;

	/*!
	 * \brief Constructor for defining a expense object.
	 * \param dt Date of the expense.
	 * \param exp Value of the expense.
	 * \param desc Description of the expense.
	 * \param sec Filename where the expense is defined in.
	 *
	 */
	Expense(
		std::string dt, float exp,
		std::string desc,std::string sec
	)
		: date {dt}
		, expense {exp}
		, description {desc}
		, section {sec}
	{};
	
	/*!
	 * \brief Function to print out the expense info in a comfortable way.
	 * \return Returns Formated expense info.
	 */
	std::string toString(){
		std::string result = section.
				append(", ").
				append(description).
				append(", ").
				append(date).
				append(", ").
				append(std::to_string(expense));
		return result;
	};
	
	//! Overwrite << operator for easily showing formated expense info.
	friend std::ostream& operator <<(std::ostream& stream, Expense& e){
		stream << e.toString();
		return stream;
	};

};
