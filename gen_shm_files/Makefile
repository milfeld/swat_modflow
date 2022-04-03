GDB=-g
all: write_shm 
	echo Done

write_shm: writer.o lib.o timers.o
	icc -o write_shm lib.o writer.o timers.o -lrt -fopenmp $(GDB)

writer.o: writer.c shared_mem.h timers.o
	icc -c writer.c -fopenmp $(GDB)

lib.o: lib.c
	icc -c lib.c $(GDB)

timers.o  : timers.c
	icc  -D _GNU_SOURCE -c $< -o $@ $(GDB)

PHONY: clean purge
purge:
	rm -rf write_shm writer.o lib.o timers.o

clean:
	rm -rf *.o write_shm
