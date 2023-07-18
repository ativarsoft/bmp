GNAT?=gcc
GNATBIND?=gnatbind
GNATLINK?=gnatlink
GNATFLAGS?=

all: libBmp.a

libBmp.a: src/*.ad[s,b]
	mkdir -p obj
	$(GNAT) $(GNATFLAGS) -c -o obj/bmp.o src/bmp.adb
	#$(GNATBIND) -n obj/bmp.ali
	ar cr bmp.a obj/bmp.o
	ranlib bmp.a

clean:
	rm -fr obj
	rm -fr lib
	rm -f *.o
	rm -f *.a
	rm -f *.ali
	rm -f b~*.ad[b,s]
	rm -f b~*.ali

.PHONY: all clean

