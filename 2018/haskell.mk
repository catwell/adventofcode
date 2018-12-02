.PHONY: all clean run1 run2

all: run1 run2

clean:
	rm -f *.hi *.o part1 part2

part1: Common.hs part1.hs
	ghc -dynamic part1.hs -o part1

part2: Common.hs part2.hs
	ghc -dynamic part2.hs -o part2

run1: part1
	./part1 ../input.txt

run2: part2
	./part2 ../input.txt
