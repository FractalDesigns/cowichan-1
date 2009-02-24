CFLAGS = -g -Wall

APP = winnow
CC = mpiCC
LIBS = -L/h/u4/c8/03/c8borzen/494/boost_mpi/lib -lboost_mpi-gcc41-mt-1_37 -lboost_serialization-gcc41-mt-1_37
INCLUDES = -I/h/u4/c8/03/c8borzen/494/boost_mpi/include/boost-1_37
OBJS = ${APP}.o serial.o parallel.o util.o
SRCS = ${APP}.cpp serial.cpp parallel.cpp ../shared/util.cpp
HDRS =  serial.h parallel.h ../include/main.h ../include/util.h ../include/generic.h ../include/type.h

all: winnow

# The variable $@ has the value of the target.
winnow: ${OBJS}
	${CC} ${CFLAGS} ${INCLUDES} -o $@ ${OBJS} ${LIBS}

winnow.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c winnow.cpp ${LIBS}

serial.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c serial.cpp ${LIBS}

parallel.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c parallel.cpp ${LIBS}

util.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c ../shared/util.cpp ${LIBS}

clean:
	rm *.o ${APP}
