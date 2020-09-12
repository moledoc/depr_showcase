//! \file main.h
#pragma once 
#include <iostream>
#include <vector>
#include <fstream> // For opening files.
#include <sstream> // For stringstream

#include "dirent.h" // https://github.com/tronkko/dirent

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "defines.h"
#include "expense.h"
#include "report.h"
#include "option.h"
#include "date.h"

#define parent_dir "/.config/expenses"

/*!
 * \brief Print out shortened help. For more information read the man page.
 *
 */
void help(){
	std::cout << ("expenses v")<< VERSION << "\n" << std::endl;
	std::cout << "Summarizes expenses data for given period\n" << std::endl;
	std::cout << "Usage: expenses [options [ yyyy-mm | yyyy-mm-dd ] [ yyyy-mm | yyyy-mm-dd ]]\n" << std::endl;
	std::cout << "Options:\n"
		<< "-c --current\n\t Summarizes the running month. This flag is used when there are no flags given.\n\n"
		<< "-f --from (yyyy-mm | yyyy-mm-dd)\n\t Summarizes expenses from the given date up to the end of running month.\n\n"
		<< "-h --help\n\t Show this help.\n\n" 
		<< "-m --month (yyyy-mm | yyyy-mm-dd)\n\t Summarizes one month expenses starting from the given date.\n\t If date is omitted, the first day of the month is used.\n\n"
		<< "--monthly\n\t Summarizes each month expenses starting with the earliest\n\t and ending with the latest defined month in the data.\n\n"
		<< "--path [path]\n\t Path to data files. Default location for data is $HOME/.config/expenses/data.\n\n"
		<< "--period (yyyy-mm | yyyy-mm-dd) (yyyy-mm | yyyy-mm-dd)\n\t Summarizes between given dates.\n\t If date is omitted, the first day of the month is used.\n\n"
		<< "-p --previous\n\t The previous month.\n\n"
		<< "-s --store\n\t Stores summary in $HOME/.config/expenses/data/store.\n\t If --path flag is used, then summaries are stored in <path to data>/store.\n\t If the directory <path to data>/store doesn't exist, it will be created.\n\t For each period a subdirectory is made.\n\t The subdirectory contains one summary file for each data file \n\t + one summary file containing info about all the files.\n\n"
		<< "-t --total\n\t Summarizes all expenses.\n"
		<< std::endl;
	std::cout << "For more information read the man page or README" << std::endl;
};

//TODO check alternative places, such as home dir.
/*
 * \brief Function to make the path to /data directory.
 * \param dir Data directory name. Default is 'data'.
 * Default directory where the data is searched for 
 * is located in $HOME/.config/expenses/data.
 * \return The path to default /data directory.
 */
std::string parse_data_path(const std::string dir = "data"){
	std::string data_path = getenv("HOME");
	data_path.append(parent_dir).append("/").append(dir);
	//data_path.append(parent_dir).append("/data");
	return data_path;
};

/*!
 * \brief Parse dates
 * \param offset Number of months that are offsetted from 
 * either current or given date. This is used in eg. with flag --previous.
 * \param dt Given date string. If date is not used,
 * then dates are calculated from current time.
 * \return Vector with date objects, that have one month difference,
 * either starting from current month or given month (+ accounting offset).
 *
 * Date objects are easier to handle and also have some nice properties.
 */
std::vector<Date> parse_date(const int offset = 0,std::string dt=""){
	int year = 0;
	int month = 0; 
	int day = 0;

	// Parse current date and take the beginning of running month.
	if(dt==""){
		time_t current = std::time(0);
		tm *date = localtime(&current);
		year = 1900+date->tm_year;
		month = 1+date->tm_mon;
		//day = date->tm_mday;
		day = 1;

	// Parse given date value.
	} else {
		std::string delimiter = "-";
		size_t pos = 0;
		
		// Catch incorrect date arguments.
		try {
			pos = dt.find(delimiter);
			year = std::stoi(dt.substr(0, pos));
			dt.erase(0, pos + delimiter.length());
			pos = dt.find(delimiter);
			month = std::stoi(dt.substr(0, pos));
		
			// If day part is omitted,
			// then take the first day of the month.
			if (pos!=std::string::npos){
				day = std::stoi(dt.substr(pos+1, std::string::npos));
			} else {
				day = 1;
			};
		} catch(std::invalid_argument){
			throw "Wrong date argument.";
		};
	};
	// Check year value.
	// Allow years between 1900 and 3000.
	if(!(year >= 1900 && year <= 3000))
		 throw "Unsupported year in date argument.";
	
	// Check month value.
	if(month > 12 || month == 0) 
		throw "Wrong month in date argument.";

	// Check day value.
	if(day>31) 
		 throw "Wrong day in date argument.";

	if(day == 31 && ((month < 8 && month%2==0) || (month > 7 && month%2!=0) ))
		 throw "Wrong day in date argument.";

	if(month == 2){
		if(day > 28 && year%4!=0) throw "Wrong day in date argument.";
		else if(day > 29) throw "Wrong day in date argument.";
	};

	// If offset is given, apply it.
	// Used in case of eg. --previous.
	if(offset != 0) month = month+offset;

	// Make date objects, that have one month difference.
	// Make lower date.
	Date lower_date = Date(year,month,day);
	
	// Deal with change of year.
	if(month == 12){
		month = 0; //because we will add +1 to month.
		year+=1;
	};

	// Deal with the end of the month.
	// If next month has 30 days.
	if(day == 31 && (month != 1 && month != 7 && month != 0))
		day-=1;
	// If next month has 31 days.
	if(day == 30 && month!=0 && ((month < 8 && month%2==0) || (month > 8 && month%2!=0)))
		day+=1;
	// February
	if(month == 1){
		if(year % 4 != 0 && day>28) day=28;
		else if(year % 4!=0 && day<=28) day+=0; //Keep it as the same day. This is more convenient than handling leap year conditions.
		// If it reaches here, then it is a leap year.
		else if(day>29) day=29;
		else day+=0;
	};	
	// If it's end of February, then take end of March.
	// If not, then take the same day in March as the upper date.
	if(month == 2){
		if(day == 28 && year%4!=0) day+=3;
		else if(day == 29) day+=2;
	};	

	Date upper_date = Date(year,month+1,day);

	std::vector<Date> bounds = {lower_date,upper_date};
	return bounds;
};

/*!
 * \brief Parses user options/flags.
 * \param argc How many parameters did the program get.
 * \param argv Given parameters.
 * \data_path Path to data files.
 * \return Option object that contains the flag (as a readable word),
 * lower and upper date (date span),
 * path to data and info whether the information is to be stored or not.
 *
 * If no flags are given, then the flag --current is used as default.
 */
Option parse_flags(int argc, char *argv[]){
	std::string data_path = parse_data_path();
	std::string flag;
	bool store = false;
	std::vector<Date> bounds;
	std::vector<Date> bounds_period;
	for(int i = 0; i < argc; i++){
		// Some flags push_back dates.
		// Since the first two dates are taken,
		// this ensures, that those dates are correct ones,
		// in case multiple flags are given, 
		// that push_back dates.
		bounds.clear();
		std::string given_option = argv[i];
		if(given_option==short_help || given_option==long_help){
			help();
			Option option;
			option.flag = "help";
			return option;
		} else if	(
			given_option==short_current || 
			given_option==long_current ||
			argc == 1
			) // If there are no parameters, then current is default.
		{
			bounds = parse_date();
			flag = "current";
			
		} else if(given_option==short_prev || given_option==long_prev){
			bounds = parse_date(-1);
			flag = "previous";

		} else if(given_option==short_total || given_option==long_total){
			bounds.push_back(Date(1900,1,1)); // Arbitrary small date
			bounds.push_back(Date(3000,12,1)); // Arbitrary big date
			flag = "total";

		} else if(given_option==period_flag){
			if (argc >= (i+3)){
				// Get the given smaller date.
				bounds_period = parse_date(0,argv[i+1]);
				bounds.push_back(bounds_period[0]); 
				// Get the given bigger date.
				bounds_period = parse_date(0,argv[i+2]);
				bounds.push_back(bounds_period[0]); 
			} else{
				throw "Incorrect number of parameters.";
			};
			if(bounds.front().date > bounds.back().date){
				throw "Incorrect date order.";
			};
			flag = "period";

		} else if(given_option==short_month || given_option==long_month){
			if (argc >= (i+2)) {
				bounds = parse_date(0,argv[i+1]);
			} else{
				throw "Incorrect number of parameters.";
			};
			flag = "month";

		} else if(given_option==short_from || given_option==long_from){
			if (argc >= (i+2)) {
				// Get the from date
				bounds_period = parse_date(0,argv[i+1]);
				bounds.push_back(bounds_period[0]); 
				// Get the upper date bound for current time.
				bounds_period = parse_date();
				bounds.push_back(bounds_period[1]); 
			} else{
				throw "Incorrect number of parameters.";
			};
			flag = "from ";

		} else if(given_option==short_store || given_option==long_store){
			store=true;

		} else if(given_option==path_flag){
			if (argc >= (i+2)) data_path = argv[i+1];

		} else if(given_option==monthly_flag){
			bounds.push_back(Date(1900,1,1)); // Arbitrary small date
			bounds.push_back(Date(3000,12,1)); // Arbitrary big date
			flag = "monthly";
		};
	};
	
	if (flag == "")	throw "Incorrect parameter.";
	
	Option option = Option(flag,bounds[0].toString(),
				bounds[1].toString(),data_path,
				store);
	return option;
};

/*!
 * \brief Make the data file filepaths.
 * \param data_path Path to data directory.
 * \return Vector with paths to data files.
 *
 * This function template was taken from the stack overflow
 * and modified to this programs needs.
 *
 * https://stackoverflow.com/questions/612097/how-can-i-get-the-list-of-files-in-a-directory-using-c-or-c
 */
std::vector<std::string> datafile_names(std::string data_path) { 
	std::vector<std::string> datafiles;
	std::string filename;

	const char * data_path0 = data_path.c_str();
	struct dirent *entry;
   	DIR *dir = opendir(data_path0);
   	
   	if (dir == NULL) {
   	   return datafiles;
   	}
	// Construct the full path to filename.
   	while ((entry = readdir(dir)) != NULL) {
		filename = entry->d_name; 
		// Let's not select current and parent directory.
		if (!(filename == ".." || filename == ".")){
			datafiles.push_back(filename);
		};
   	}
   	closedir(dir);
	return datafiles;
}

/*!
 * \brief For each expense make an expense object.
 * \param datafiles Vector of datafiles, where the expenses are.
 * \param option Contains lower and upper dates, flag, data directory path.
 * \return The pointer to vector of expenses.
 */
std::vector<Expense> make_Expenses
	(
	std::vector<std::string> datafiles,
	const Option& option
	)
{
	std::vector<Expense> expenses;

	std::ifstream file;
	std::string line;
	std::string full_path;

	float tmp_expense;
	std::string tmp_description;
	std::string tmp_filename;
	std::string tmp_date;

	for (std::string tmp_filename : datafiles){
		full_path = option.data_path;
		full_path.append("/").
			append(tmp_filename);
	
		// Open file
		file.open(full_path,std::ios::in);

		// First line in data file should be headers.
		while (std::getline(file, line)) {
			// write values in tmp variables.
			file >> tmp_date >> tmp_expense >> tmp_description;

			// If date conditions are met, make the expense object.
			if (tmp_date >= option.lower_date &&
				tmp_date <= option.upper_date){
				
				expenses.push_back(Expense(
					tmp_date, tmp_expense,
					tmp_description,tmp_filename
				));
			};
		};
		// Close file
		file.close();
	};
	return expenses;
}

/*!
 * \brief Sort expenses into unordered map by filename (section).
 * \param expenses Vector of expenses.
 * \return Map, where is expense is sorted by it's filename (section).
 */
std::unordered_map<std::string,std::vector<Expense> > sort_expenses
(const std::vector<Expense> expenses)
{
	std::vector<Expense> tmp;
	// Make unordered list. 
	// Also initialize section 'all'.
	std::unordered_map<std::string,std::vector<Expense> > sorted_exp = {{"all",tmp}};
	// Sort the expenses based on the section.
	for (Expense expense : expenses){
		// TODO: is there a way without if-else,
		// because I'm repeating the code atm.
		if(sorted_exp.find(expense.section) == sorted_exp.end()){
			
			// Clean tmp vector just in case.
			tmp.clear();	

			// Push new instance into the vector
			// for initializing it.

			tmp.push_back(expense);
			sorted_exp.insert({expense.section,tmp});
			sorted_exp.at("all").push_back(expense);

		} else {
			sorted_exp.at(expense.section).push_back(expense);
		      	sorted_exp.at("all").push_back(expense);
		};

	};
	return sorted_exp;
};

/*!
 * \brief Make vector of reports, so that they can be outputted 
 * in the main function.
 * \param Option Parsed user flags.
 * \param expenses Vector of Expenses.
 * \return Vector of Reports.
 */
std::vector<Report> make_Report
(const Option option, const std::vector<Expense> expenses)
{
	// Sort the expenses based on the section.
	std::unordered_map<std::string,std::vector<Expense> > sorted_exp = sort_expenses(expenses);
	
	std::vector<Report> reports;
	
	// Make report for each filename (section)
	// and store them in a vector.
	for (const auto &sec : sorted_exp) {
		
		Report report;
		report.section = sec.first;
		report.flag = option.flag;
		report.date_period = option.lower_date;
		report.date_period.append(" -- ").append(option.upper_date);
		report.total_expenses = summed_total(sec.second); 
		report.expenses_by_desc = summed_by_desc(sec.second); 

		reports.push_back(report);
	};
	
	return reports;
}

/*! 
 * \brief Help function to control, whether the directory to given path exists.
 * \param path The directory we want to check, whether it exists.
 *
 */
void directory_exist(const std::string path){
	// Following if-else found at:
	//https://stackoverflow.com/questions/12510874/how-can-i-check-if-a-directory-exists
	// and modified for current use.
	DIR* dir = opendir(path.c_str());
	if (dir) {
	    	/* Directory exists. */
	    	closedir(dir);
	} else if (ENOENT == errno) {
	    	/* Directory does not exist. */
		// Helpful ling: https://techoverflow.net/2013/04/05/how-to-use-mkdir-from-sysstat-h/
		mkdir(path.c_str(),0777);
		std::cout << "Directory '" 
				<< path 
				<< "' created.\n"
				<< std::endl;
	};

}

/*!
 * \brief Store given report.
 * \param report The report, that is being restored.
 * \param data_path The path to data.
 * Summaries are stored in a subdirectory inside the /data directory.
 * 
 * The report is stored in $HOME/parent_dir/store/<report date period>.
 * For each file in $HOME/parent_dir/data a store file is created.
 * Additional store file is created, where data for all the files is stored.
 */
void storing(Report report, const std::string data_path){
	//std::string store_path = parse_data_path("/store/") + report.date_period;
	
	std::string store_path = data_path+"/store/" + report.date_period;
	
	// Check if 'store' path exists.
	directory_exist(data_path+"/store/");
	// Check if subdirectory for summary exists.
	directory_exist(store_path);

	// Store given report.
	store_path.append("/").append(report.section);
	std::ofstream out(store_path);
	out << report.toString();
	out.close();
};
