CC=/usr/bin/g++
CFLAGS=
OUT=vliwEncoder

OBJS=vliwEncoder.o

all:$(OUT)

clean:
	rm -r *.o $(OUT)

$(OUT):$(OBJS)
	$(CC) -o $(OUT) $(OBJS)

%.o:%.cpp
	$(CC) $(CFLAGS) -c $< -o $@

vliwEncoder.o: vliwEncoder.cpp
