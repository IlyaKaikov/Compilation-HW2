.PHONY: all clean

CC = g++
CFLAGS = -std=c++17

all: clean
	bison -Wcounterexamples -d parser.y
	flex scanner.lex
	$(CC) $(CFLAGS) -o hw2 *.c *.cpp
clean:
	rm -f lex.yy.* parser.tab.* hw2
