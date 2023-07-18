all: libBmp.a

libBmp.a: src/*.ad[s,b]
	alr build

clean:
	alr clean

.PHONY: all clean

