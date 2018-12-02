.PHONY: all run1 run2

all: run1 run2

run1:
	lua part1.lua ../input.txt

run2:
	lua part2.lua ../input.txt
