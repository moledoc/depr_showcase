#include <iostream>
#include <cstdlib>  // For EXIT_SUCCESS and other constants.
#include <ctime> // For time functions.
#include <string>

#include "main.h"


int main(int argc, char *argv[]){
	// Define some help variables for when --monthly is used.
	std::string end;
	std::vector<Date> tmp;	

	// Parse flags
	Option option;
	try{
		option = parse_flags(argc,argv);
	} catch (const char* msg){
		std::cerr << msg << std::endl;
		return EXIT_FAILURE;
	};

	if(option.flag == "help") return EXIT_SUCCESS;

	// Get datafile paths.
	std::vector<std::string> datafiles = datafile_names(option.data_path);	

	// Get expenses info.
	std::vector<Expense> expenses = make_Expenses(datafiles,option);
	
	// Deal with lower/upper date in case of --total or --monthly
	if(option.flag == "total" || option.flag == "monthly"){
		std::string min = "3000-01-01";
		std::string max = "1900-01-01";
		for (Expense check : expenses){
			if(check.date <= min) min=check.date;
			if(check.date >= max) max=check.date;
		};
		option.lower_date = min;
		option.upper_date = max;
		if(option.flag == "monthly"){
			// In this point, all dates are in format yyyy-mm-dd.
			// Truncate lower date to it's first day of the month.
			// Make upper date the first day of the next month 
			// of the latest expense.
			tmp = parse_date(0,min.substr(0,7));
			option.lower_date = tmp.front().toString();
			option.upper_date = tmp.back().toString();
			end = parse_date(0,max.substr(0,7)).back().toString();
		};
	};
	// Only necessary for --monthly flag.
	// If any other flag is used,
	// then it only does one cycle.	
	while(true){
		// Get reports	
		std::vector<Report> reports = make_Report(option,expenses);
		
		// Print the reports
		for (Report report : reports){
			// If store flag used, then store the reports.
			if(option.store){
				storing(report,option.data_path);
			};
			std::cout << report << std::endl;
		};
		
		if(option.flag != "monthly" || option.upper_date == end){
			break;
		} else {
			tmp.clear();
			tmp = parse_date(0,option.upper_date);
			option.lower_date = tmp.front().toString();
			option.upper_date = tmp.back().toString();
		};
	};

	return EXIT_SUCCESS;
};

