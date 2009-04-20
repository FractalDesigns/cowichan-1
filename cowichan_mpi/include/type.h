/*==============================================================*/
/* generic/hdr/type.h : generic type definitions		*/
/*==============================================================*/

typedef unsigned char boolean;		/* boolean */
//typedef double real;			/* double-precision reals */
typedef float real;			/* single-precision reals */

#define FMT_REAL_ERR "%r"
#define FMT_REAL_RD "%le"
#define FMT_REAL_WR "%24.16e\n"

#define MAXEXT 7000

struct pt {
private:
    friend class boost::serialization::access;

    template<class Archive>
    void serialize(Archive & ar, const unsigned int version)
    {
        ar & x;
        ar & y;
        ar & w;
    }
public:
  real x, y; /* point location */
  int  w;    /* weight */
};

BOOST_IS_MPI_DATATYPE(pt)
BOOST_CLASS_TRACKING(pt,track_never)
BOOST_IS_BITWISE_SERIALIZABLE(pt)

#define FMT_PT_RD "%le%le%d"
#define FMT_PT_WR "%e\t%e\t%d\n"

typedef boolean bool1D;
typedef boolean bool1DX;
typedef boolean bool2D [MAXEXT];

typedef int int1D;
typedef int int1DX;
typedef int int2D [MAXEXT];

typedef pt pt1D;
typedef pt pt1DX;

typedef real real1D;
typedef real real1DX;
typedef real real2D[MAXEXT];
