//! \file option.h
#pragma once
#include <iostream>
#include <string>

class Option{
public:
	//! Flag that was given by the user.
	std::string flag;
	//! Starting date of the summarizing period.
	std::string lower_date;
	//! Ending date of the summarizing period.
	std::string upper_date;
	//! Path to data directory.
	std::string data_path;
	//! Indicator, whether summary is to be stored or not.
	bool store;

	//! \brief Default constructor.
	Option(){}
		
	/*!
	 * \brief Constructor for defining a option object.
	 * \param fl Flag that was given by the user.
	 * \param lower Starting date of the summarizing period.
	 * \param upper Ending date of the summarizing period.
	 * \param pth Path to data directory.
	 *
	 */
	Option(
	std::string fl,std::string lower,
	std::string upper,std::string pth,
	bool stor
	)
		: flag {fl}
                , lower_date {lower}
                , upper_date {upper}
                , data_path {pth}
                , store {stor}
	{}
};
