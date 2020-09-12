VERSION = 0.1
#####################################################
# You may need to change the parameters under here. #
#####################################################

# Step 1: Choose a compiler. By default, use clang++

# If you use clang++, make sure the following line does not start with a comment (#)
CXX=clang++
# If you use g++, uncomment the following line
#CXX=g++

# Set default compiler parameters
# -Wall 	shows all warnings when compiling, always use this!
# -std=c++11 	enables the C++11 standard mode
CXXFLAGS = -Wall -std=c++11
APP_CXXFLAGS = $(CXXFLAGS) -Iinclude

# Step 2: If you use clang++ under Mac OS X, remove these comments
#CXXFLAGS += -stdlib=libc++
#LFLAGS += -stdlib=libc++

##############
# Declarations
##############

PREFIX = /usr

MANPREFIX=/usr/share/man
#${PREFIX}/man

PROGRAM = bin/expenses

OBJ = obj/main.o

DOC = docs

DIRS =  bin obj

DATA = data
STORE = store

# Build program
# This is the first target. It will be built when you run 'make' or 'make build'
build: $(PROGRAM)

dirs: 
	@mkdir -p ${DIRS}

# Do not run with sudo.
# It will make the folder in /root/.
setup: 
	@echo Creating \"${HOME}/.config/expenses/${DATA}\" \"${HOME}/.config/expenses/${DATA}/${STORE}\"
	@mkdir -p "${HOME}/.config/expenses/${DATA}/${STORE}"
	@echo Coping examples to \"${HOME}/.config/expenses/${DATA}\"
	@cp -f example/* "${HOME}/.config/expenses/${DATA}"

# Rules for compiling.
obj/main.o: src/main.cpp include/main.h include/expense.h include/report.h
	$(CXX) $(APP_CXXFLAGS) $(CXXFLAGS) -c src/main.cpp -o obj/main.o 

# Rule for linking.
$(PROGRAM): dirs $(OBJ)
	$(CXX) $(OBJ) $(LFLAGS) -o $(PROGRAM)

# Documentation
doc:
	doxygen

# Clean directory of unnecessary files.
clean:
	rm -f $(OBJ) $(PROGRAM)
	rm -rf docs ${DIRS}

# Make doxygen documentation and build program.
all: build

# Installation
install: all
	@echo Installing executable file to ${PREFIX}/bin
	@mkdir -p ${PREFIX}/bin
	@cp -f bin/expenses ${PREFIX}/bin
	@chmod 755 ${PREFIX}/bin/expenses
	@chmod +x ${PREFIX}/bin/expenses
	@echo Installing manual page to ${MANPREFIX}/man1
	@mkdir -p ${MANPREFIX}/man1
	@sed "s/VERSION/${VERSION}/g" <expenses.1 >${MANPREFIX}/man1/expenses.1
	@chmod 644 ${MANPREFIX}/man1/expenses.1

# Uninstallation
uninstall:clean
	@echo removing executable file from ${PREFIX}/bin
	@rm -f ${PREFIX}/bin/expenses
	@echo removing manual page from ${MANPREFIX}/man1
	@rm -f ${MANPREFIX}/man1/expenses.1
