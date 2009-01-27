// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <time.h>

#if defined(WIN32)   // Windows
  #include <tchar.h>
#else                // Linux
  #define _T(arg) arg
  #define TCHAR char
  #define _TCHAR char
#endif               // end of WIN32/Linux definitions

#include "../include/type.h"
#include "../include/generic.h"
#include "../include/util.h"

// TODO: reference additional headers your program requires here
