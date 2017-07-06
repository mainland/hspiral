CFLAGS = -I. -march=native -mtune=native -Ofast -fno-tree-vectorize

LDFLAGS = -lfftw3

OPCOUNTS = ../opcounts

OPCOUNTS_FLAGS = -fmax-unroll=4096

DFTGEN = ../search

DFTGEN_FLAGS = -fmax-unroll=1024

SPIRAL_SRC = \
	spiral/spiral_fft_double.c \
	spiral/spiral_private.c

HSPIRAL_SRC = \
	hspiral/fft2.c \
	hspiral/fft4.c \
	hspiral/fft8.c \
	hspiral/fft16.c \
	hspiral/fft32.c \
	hspiral/fft64.c \
	hspiral/fft128.c \
	hspiral/fft256.c \
	hspiral/fft512.c \
	hspiral/fft1024.c

.PHONY : all
all : benchmark

.PHONY : clean
clean :
	rm -f benchmark

.PHONY : hspiral
hspiral : $(HSPIRAL_SRC)

hspiral/fft%.c :
	$(DFTGEN) $(DFTGEN_FLAGS) -o $@ $*

benchmark : src/main.c $(SPIRAL_SRC) $(HSPIRAL_SRC) $(FFTW3_SRC)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

data/opcount.csv : $(OPCOUNTS)
	$(OPCOUNTS) $(OPCOUNTS_FLAGS) >$@

data/opcount-3mult.csv : $(OPCOUNTS)
	$(OPCOUNTS) $(OPCOUNTS_FLAGS) -fthree-mult >$@

data/timing.csv : benchmark
	./benchmark >$@

flops.pdf : bin/plot.R data/timing.csv
	./bin/plot.R --hspiral data/timing.csv --clock-rate 3.4 --format pdf
