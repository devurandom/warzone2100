// Written by Per I Mathisen, 2007
// Released into the public domain, no rights reserved.
//
// The tagfile format is portable, nested, tagged binary format that should be
// useful for storing information that needs to be accessible several years
// into the future. That is, it is an easily extensible binary format, a feature
// usually reserved for text based formats. It also uses default values to 
// reduce space, and despite its tagged nature, it consumes only a minimal overhead
// through the use a well-defined protocol file that is loaded separately.
//
// Each user of this code should define a protocol that is henceforth called
// the defined format. This consists of a series of tags that may contain either
// information or new tag groups. The tag groups can be nested until you run out
// of memory. Each tag group has its own namespace of tags. Each tag has a defined
// value representation (VR), a defined value multiplicity (VM), and an optional 
// default value.
//
// Tags that have the default value may be omitted, and the read code will simply
// insert the default value when the tag is read. A group must be present in order
// for the read code to enter it even if all tags inside have default values.
//
// Each group can contain multiple instances of the same tag set. These are
// separated by separator tags. You can have any number of instances in a group,
// but you must know the number of instances beforehand.
//
// When reading and writing tags, remember to ALWAYS do so with successively 
// increasing tag value. Do not write tag number 3 before tag number 1.
//
// See the included defined format files for more information on how to write them.

#ifndef _tagfile_h
#define _tagfile_h

#include <stdio.h>
#include <physfs.h>

#include "lib/framework/types.h"

typedef uint8_t element_t;

/* Open definition file and data file; return true if successful. */
bool tagOpenWrite(const char *definition, const char *datafile);
bool tagOpenRead(const char *definition, const char *datafile);

/* Clean up and close the tagfile system down. */
void tagClose();

/* Report last error, then zeroes the internal error variable. If it
 * returns false, there is no error. */
bool tagGetError();

/* Output a more verbose string about the last error, given by tag_errno. */
const char *tagGetErrorString();

/* Built-in unit test */
void tagTest(void);

/*** tagWrite ***/

/* Enter, leave and jump to next instance of tags in a group. */
bool tagWriteEnter(element_t tag, uint16_t elements);
bool tagWriteLeave(element_t tag); // pass in group we are leaving for error checking
bool tagWriteSeparator();

/* Write methods */
bool tagWrite(element_t tag, uint32_t val);
bool tagWrites(element_t tag, int32_t val);
bool tagWritef(element_t tag, float val);
bool tagWritefv(element_t tag, uint16_t count, float *vals);
bool tagWrite8v(element_t tag, uint16_t count, uint8_t *vals);
bool tagWrite16v(element_t tag, uint16_t count, uint16_t *vals);
bool tagWrites32v(element_t tag, uint16_t count, int32_t *vals);
bool tagWriteString(element_t tag, const char *string);
bool tagWriteBool(element_t tag, bool val);

/*** tagread ***/

/* Enter, leave and jump to next instance of tags in a group. */
uint16_t tagReadEnter(element_t tag);
void tagReadLeave(element_t tag); // pass in group we are leaving for error checking
bool tagReadNext(); // false if no more entities to read

/* Read methods */
uint32_t tagRead(element_t tag);
int32_t tagReads(element_t tag);
bool tagReadBool(element_t tag);
float tagReadf(element_t tag);
bool tagReadfv(element_t tag, uint16_t size, float *vals);
uint8_t *tagRead8vDup(element_t tag, int *size);
bool tagRead8v(element_t tag, uint16_t size, uint8_t *vals);
bool tagRead16v(element_t tag, uint16_t size, uint16_t *vals);
bool tagReads16v(element_t tag, uint16_t size, int16_t *vals);
bool tagReadString(element_t tag, uint16_t size, char *buffer);
char *tagReadStringDup(element_t tag);

#endif
