GNAT?=gcc
GNATBIND?=gnatbind
GNATLINK?=gnatlink
GNATFLAGS?=

all: libadabmp.a

libadabmp.a: src/*.ad[s,b]
	mkdir -p obj
	$(GNAT) $(GNATFLAGS) -c -o obj/bmp.o src/bmp.adb
	ar cr libadabmp.a obj/bmp.o
	ranlib libadabmp.a

clean:
	rm -fr obj
	rm -fr lib
	rm -f *.o
	rm -f *.a
	rm -f *.ali
	rm -f b~*.ad[b,s]
	rm -f b~*.ali

install:

.PHONY: all clean install

