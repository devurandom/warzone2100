#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#define stricmp _stricmp
#define strnicmp _strnicmp
#define strlwr _strlwr
#ifndef _DEBUG
# define strdup _strdup // CRT HEAP debugging defines this also
#endif

#define vsnprintf _vsnprintf
#define snprintf  _snprintf

#define fileno _fileno

#define inline __inline