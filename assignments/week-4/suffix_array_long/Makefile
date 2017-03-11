FLAGS:=-pipe -O2 -std=c++0x -g -ggdb
#
all: suffix_array_long suffix_array_long_debug

suffix_array_long_debug: suffix_array_long.cc
	g++  $(FLAGS) suffix_array_long.cc -DDEBUG -lm -o ./bin/suffix_array_long_debug

suffix_array_long: suffix_array_long.cc
	g++  $(FLAGS) suffix_array_long.cc -lm -o ./bin/suffix_array_long
