CFLAGS = -g -Wall

CC = g++
LIBS = 
INCLUDES = 
OBJS = serial.o ../shared/stdafx.o ../shared/util.o
SRCS = randmat.cpp serial.cpp ../shared/stdafx.cpp ../shared/util.cpp
HDRS =  serial.h ../include/stdafx.h ../include/util.h

all: randmat

# The variable $@ has the value of the target.
randmat: randmat.o ${OBJS}
	${CC} ${CFLAGS} ${INCLUDES} -o $@ randmat.o ${OBJS} ${LIBS}

.c.o:
	${CC} ${CFLAGS} ${INCLUDES} -c $<

depend: 
	makedepend ${SRCS}

clean:
	rm *.o
