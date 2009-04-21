CFLAGS = -O2 -g -Wall

APP = product
CC = mpiCC
LIBS = -L/home/aborzenko/cowichan/boost/lib -lboost_mpi-gcc41-mt-1_37 -lboost_serialization-gcc41-mt-1_37
INCLUDES = -I/home/aborzenko/cowichan/boost/include/boost-1_37
OBJS = ${APP}.o serial.o parallel.o util.o
SRCS = ${APP}.cpp serial.cpp parallel.cpp ../shared/util.cpp
HDRS =  serial.h parallel.h ../include/main.h ../include/util.h ../include/generic.h ../include/type.h

all: product

# The variable $@ has the value of the target.
product: ${OBJS}
	${CC} ${CFLAGS} ${INCLUDES} -o $@ ${OBJS} ${LIBS}

product.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c product.cpp ${LIBS}

serial.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c serial.cpp ${LIBS}

parallel.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c parallel.cpp ${LIBS}

util.o: ${SRCS} ${HDRS}
	${CC} ${CFLAGS} ${INCLUDES} -c ../shared/util.cpp ${LIBS}

clean:
	rm *.o ${APP}
