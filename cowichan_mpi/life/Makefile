CFLAGS = -g -Wall

CC = g++
LIBS = 
INCLUDES = 
OBJS = serial.o ../shared/stdafx.o ../shared/util.o
SRCS = life.cpp serial.cpp ../shared/stdafx.cpp ../shared/util.cpp
HDRS =  serial.h ../include/stdafx.h ../include/util.h

all: life

# The variable $@ has the value of the target.
life: life.o ${OBJS}
	${CC} ${CFLAGS} ${INCLUDES} -o $@ life.o ${OBJS} ${LIBS}

.c.o:
	${CC} ${CFLAGS} ${INCLUDES} -c $<

depend: 
	makedepend ${SRCS}

clean:
	rm *.o
