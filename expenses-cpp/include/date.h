//! \file date.h
#pragma once
#include <iostream>
#include <string>

class Date{

public:
	//! The year value of a date.
	const int year;
	//! The month value of a date.
	const int month;
	//! The day value of a date.
	const int day;
	//! The date value in one continuous string.
	const std::string date;
	
	/*!
	 * \brief Constructor for defining a date object.
	 * \param y The year of the date.
	 * \param m The month of the date.
	 * \param d The day of the month.
	 *
	 */
	Date(const int y,const int m, const int d)
		: year {y}
		, month {m}
		, day {d}
		, date {std::to_string(y).
			append(std::to_string(m)).
			append(std::to_string(d))}
	{}
	
	/*!
	 * \brief Function to print out date in format yyyy-mm-dd.
	 * \return Returns date in the format of yyyy-mm-dd.
	 */
	std::string toString(){
		std::string m_0;
		std::string d_0;
		std::string date_toString;
		
		if ( month<10) m_0.append("0");
		if ( day<10) d_0.append("0");

		date_toString.append(std::to_string(year)).
			append("-").
			append(m_0).
			append(std::to_string(month)).
			append("-").
			append(d_0).
			append(std::to_string(day));
		return date_toString;
	};
	
	//! Overwrite << operator for easily showing date object.
	friend std::ostream& operator <<(std::ostream& stream, Date& dt){
		stream << dt.toString();
		return stream;
	};
};
