CFLAGS = -g -Wall

APP = life
CC = g++
LIBS = 
INCLUDES = 
OBJS = serial.o parallel.o ../shared/util.o
SRCS = ${APP}.cpp serial.cpp parallel.cpp ../shared/util.cpp
HDRS =  serial.h parallel.h ../include/main.h ../include/util.h

all: life

# The variable $@ has the value of the target.
life: ${APP}.o ${OBJS}
	${CC} ${CFLAGS} ${INCLUDES} -o $@ ${APP}.o ${OBJS} ${LIBS}
	
clean:
	rm *.o
