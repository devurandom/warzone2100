// Written by Per I Mathisen, 2007
// Released into the public domain, no rights reserved.
// ANSI C + stdint.h

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "frame.h"

#include "tagfile.h"

#define TAG_SEPARATOR 0
#define TAG_GROUP_END 255

enum internal_types
{
	TF_INT_U8, TF_INT_U16, TF_INT_U32, TF_INT_S8, TF_INT_S16, TF_INT_S32, TF_INT_FLOAT, 
	TF_INT_U16_ARRAY, TF_INT_FLOAT_ARRAY, TF_INT_U8_ARRAY, TF_INT_GROUP, TF_INT_BOOL
};

struct define
{
	int16_t vm;
	uint8_t element;
	char vr[2];
	struct define *parent;	// parent group
	struct define *group;	// child group
	struct define *next;	// sibling group
	struct define *current; // where in the sibling list we currently are
	bool defaultval;
	union {
		uint32_t uint32_tval;
		int32_t int32_tval;
		float floatval;
	} val;
};

static bool tag_error = false;
static struct define *first = NULL;
static struct define *current = NULL;
static int line = 0; // report in error message
static bool readmode = true;
static char *bufptr = NULL;
static PHYSFS_file *handle = NULL;
#define ERR_BUF_SIZE 200
static char errbuf[ERR_BUF_SIZE];

#define TF_ERROR(...) \
do { \
	tag_error = true; \
	snprintf(errbuf, sizeof(errbuf), __VA_ARGS__); \
	errbuf[sizeof(errbuf) - 1] = '\0'; /* Guarantee to nul-terminate */ \
	assert(false); \
} while(0)

// function to printf into errbuf the calling strack for nested groups on error
#define PRNG_LEN 40 // low value to avoid stack overflow
static void print_nested_groups(struct define *group)
{
	struct define *parent;
	char groupdesc[PRNG_LEN];

	if (group == NULL)
	{
		return;
	}
	parent = group->parent;

	if (parent != NULL)
	{
		snprintf(groupdesc, PRNG_LEN, "\nNested inside element %d", (int)parent->element);
		strncat(errbuf, groupdesc, sizeof(errbuf) - strlen(errbuf) - 1);
		print_nested_groups(parent);
	}
	if (parent == NULL)
	{
		strncat(errbuf, "\n", sizeof(errbuf) - strlen(errbuf) - 1);
	}
}

// returns true on success
static bool scan_defines(struct define *node, struct define *group)
{
	bool group_end = false;

	while (*bufptr != '\0' && !group_end)
	{
		int count, retval;
		unsigned int readelem;
		char vr[3];

		memset(vr, 0, sizeof(vr));
		node->group = NULL;
		node->parent = group;
		node->val.uint32_tval = 0;
		node->vm = 0;
		node->next = NULL;
		node->element = 0xFF;

		// check line endings
		while (*bufptr == '\n' || *bufptr == '\r')
		{
			if (*bufptr == '\n')
			{
				line++;
			}
			bufptr++;
		}
		// remove whitespace
		while (*bufptr == ' ' || *bufptr == '\t')
		{
			bufptr++;
		}
		// check if # comment line or whitespace
		if (*bufptr == '#' || *bufptr == '\0')
		{
			while (*bufptr != '\n' && *bufptr != '\0')
			{
				bufptr++; // discard rest of line
			}
			continue; // check empty lines and whitespace again
		}

		retval = sscanf(bufptr, "%x %2s %hd%n", &readelem, (char *)&vr, &node->vm, &count);
		node->element = readelem;
		while (retval <= 0 && *bufptr != '\n' && *bufptr != '\0') /// TODO: WTF?
		{
			printf("WTF\n");
			bufptr++; // not a valid line, so discard it
		}
		if (retval < 3)
		{
			TF_ERROR("Bad definition, line %d (retval==%d)", line, retval);
			return false;
		}
		node->vr[0] = vr[0];
		node->vr[1] = vr[1];
		bufptr += count;
		if (node->vr[0] == 'S' && node->vr[1] == 'I')
		{
			retval = sscanf(bufptr, " %d", &node->val.int32_tval);
		}
		else if ((node->vr[0] == 'U' && node->vr[1] == 'S') || (node->vr[0] == 'B' && node->vr[1] == 'O'))
		{
			retval = sscanf(bufptr, " %u", &node->val.uint32_tval);
		}
		else if (node->vr[0] == 'F' && node->vr[1] == 'P')
		{
			retval = sscanf(bufptr, " %f", &node->val.floatval);
		}
		else if ((node->vr[0] == 'S' && node->vr[1] == 'T')
		         || (node->vr[0] == 'G' && node->vr[1] == 'R')
		         || (node->vr[0] == 'E' && node->vr[1] == 'N'))
		{
			retval = 0; // these do not take a default value
		}
		else
		{
			TF_ERROR("Invalid value representation %c%c line %d", node->vr[0], node->vr[1], line);
			return false;
		}
		node->defaultval = (retval == 1);
		while (*bufptr != '\n' && *bufptr != '\0')
		{
			bufptr++; // discard rest of line
		}

		if (node->vr[0] == 'G' || node->vr[1] == 'R')
		{
			bool success;

			node->group = malloc(sizeof(*node));
			success = scan_defines(node->group, node);

			if (!success)
			{
				return false; // error
			}
		}
		else if (node->vr[0] == 'E' || node->vr[1] == 'N')
		{
			group_end = true; // un-recurse now
		}
		if (*bufptr != '\0' && !group_end)
		{
			node->next = malloc(sizeof(*node));
			node = node->next;
		}
	}
	node->next = NULL; // terminate linked list
	return true;
}

static bool init(const char *definition, const char *datafile, bool write)
{
	PHYSFS_file *fp;
	PHYSFS_sint64 fsize, fsize2;
	char *buffer;

	errbuf[0] = '\0';
	tag_error = false;
	line = 1;
	fp = PHYSFS_openRead(definition);
	if (!fp)
	{
		TF_ERROR("Error opening definition file %s: %s", definition, PHYSFS_getLastError());
		return false;
	}
	
	fsize = PHYSFS_fileLength(fp);
	assert(fsize > 0);

	buffer = bufptr = malloc(fsize + 1);
	if (!buffer || !bufptr)
	{
		debug(LOG_ERROR, "init(): Out of memory");
		abort();
		return false;
	}

	bufptr[fsize] = '\0'; // ensure it is zero terminated

 	fsize2 = PHYSFS_read(fp, bufptr, 1, fsize);
	if (fsize != fsize2)
	{
		TF_ERROR("Could not read definitions: %s", PHYSFS_getLastError());
		return false;
	}
	current = NULL; // keeps track of parent group below
	first = malloc(sizeof(*first));
	first->parent = NULL;
	first->group = NULL;
	first->next = NULL;
	first->current = NULL;
	scan_defines(first, NULL);
	free(buffer);
	bufptr = NULL;
	PHYSFS_close(fp);

	if (write)
	{
		fp = PHYSFS_openWrite(datafile);
	}
	else
	{
		fp = PHYSFS_openRead(datafile);
	}
	if (!fp)
	{
		TF_ERROR("Error opening data file %s: %s", definition, PHYSFS_getLastError());
		return false;
	}
	readmode = !write;
	current = first;
	handle = fp;
	return true;
}

bool tagOpenWrite(const char *definition, const char *datafile)
{
	return init(definition, datafile, true);
}

bool tagOpenRead(const char *definition, const char *datafile)
{
	return init(definition, datafile, false);
}

static void remove_defines(struct define *df)
{
	struct define *iter;

	for (iter = df; iter != NULL;)
	{
		struct define *del = iter;

		if (iter->group)
		{
			remove_defines(iter->group);
		}
		iter = iter->next;
		free(del);
	}
}

void tagClose()
{
	PHYSFS_close(handle);
	remove_defines(first);
}

bool tagGetError()
{
	return tag_error;
}

const char *tagGetErrorString()
{
	print_nested_groups(current);
	return errbuf;
}

static bool scan_to(element_t tag)
{
	if (tag == TAG_SEPARATOR)
	{
		return true; // does not exist in definition
	}
	// Set the right node
	for (; current->next && current->element < tag; current = current->next);
	if (current->element != tag)
	{
		TF_ERROR("Unknown element %d sought", (int)tag);
		return false;
	}
	return true;
}


/*********  TAGREAD *********/


// scan forward to after given tag (if read case, we may skip
// several tags to get there)
static bool scanforward(element_t tag)
{
	element_t read_tag;
	uint8_t tag_type;
	PHYSFS_sint64 readsize = 0, fpos;
	uint16_t array_size;

	assert(readmode);
	if (tag_error || current == NULL || !readmode)
	{
		return false;
	}

	// Skip in this group until we have reached destination tag
	if (!scan_to(tag) || PHYSFS_eof(handle))
	{
		return false; // error, or try default value
	}
	readsize = PHYSFS_read(handle, &read_tag, 1, 1);
	while (readsize == 1)
	{
		if (read_tag == tag)
		{
			assert(current->element == tag || tag == TAG_SEPARATOR);
			return true;
		}
		else if (read_tag == TAG_GROUP_END
			 || read_tag == TAG_SEPARATOR
			 || read_tag > tag)
		{
			// did not find it
			break;
		}

		/* If we got down here, we found something that we need to skip */

		readsize = PHYSFS_read(handle, &tag_type, 1, 1);
		if (readsize != 1)
		{
			TF_ERROR("Error reading tag type when skipping: %s", PHYSFS_getLastError());
			return false;
		}

		/* Skip payload */

		switch (tag_type)
		{
		case TF_INT_U16_ARRAY:
		case TF_INT_FLOAT_ARRAY:
		case TF_INT_U8_ARRAY:
			readsize = PHYSFS_read(handle, &array_size, 1, 1);
			if (readsize != 1)
			{
				TF_ERROR("Error reading tag length when skipping: %s", PHYSFS_getLastError());
				return false;
			}
			break;
		default:
			array_size = 1;
		}

		fpos = PHYSFS_tell(handle);
		switch (tag_type)
		{
		case TF_INT_U8_ARRAY:
		case TF_INT_U8:
		case TF_INT_S8: fpos += 1 * array_size; break;
		case TF_INT_FLOAT:
		case TF_INT_U16_ARRAY:
		case TF_INT_FLOAT_ARRAY:
		case TF_INT_U16:
		case TF_INT_S16: fpos += 2 * array_size; break;
		case TF_INT_U32:
		case TF_INT_S32: fpos += 4 * array_size; break;
		case TF_INT_GROUP: fpos += 2; break;
		default:
			TF_ERROR("Invalid value type in buffer");
			return false;
		}

		PHYSFS_seek(handle, fpos);

		/* New element ready now, repeat loop */
		readsize = PHYSFS_read(handle, &read_tag, 1, 1);
	}
	if (readsize != 1)
	{
		TF_ERROR("Could not read tag: %s", PHYSFS_getLastError());
		return false;
	}
	else
	{
		// If we come here, we failed to find the tag. Roll back the last element found, and
		// check for default value.
		fpos = PHYSFS_tell(handle);
		fpos -= sizeof(element_t);
		PHYSFS_seek(handle, fpos);
		assert(current != NULL);
		assert(current->element == tag);

		if (!current->defaultval)
		{
			TF_ERROR("Tag not found and no default: %d", (int)tag);
			return false;
		}
	}

	return false; // tag not found before end of instance, group or file
}

bool tagReadNext()
{
	// scan forward until we find a separator value element tag in this group
	if (!scanforward(TAG_SEPARATOR))
	{
		return false;
	}
	if (current->parent == NULL)
	{
		current = first; // topmost group
	}
	else
	{
		current = current->parent->group; // reset to start of group tag index
	}
	return true;
}

uint16_t tagReadEnter(element_t tag)
{
	uint16_t elements;
	element_t tagtype;

	assert(readmode);
	if (!scanforward(tag) || !readmode)
	{
		return 0; // none found; avoid error reporting here
	}
	if (!PHYSFS_readUBE8(handle, &tagtype))
	{
		TF_ERROR("Error reading group type: %s", PHYSFS_getLastError());
		return 0;
	}
	if (tagtype != TF_INT_GROUP)
	{
		TF_ERROR("Error in group VR, tag %d", tag);
		return 0;
	}
	if (!PHYSFS_readUBE16(handle, &elements))
	{
		TF_ERROR("Error accessing group size: %s", PHYSFS_getLastError());
		return 0;
	}
	if (!current->group)
	{
		TF_ERROR("Cannot enter group, none defined for element %d!", (int)current->element);
		return 0;
	}
	assert(current->group->parent != NULL);
	assert(current->element == tag);
	current->group->parent->current = current; // save where we are. ok, this is very contrived code.
	current = current->group;
	return elements;
}

void tagReadLeave(element_t tag)
{
	if (!scanforward(TAG_GROUP_END))
	{
		TF_ERROR("Cannot leave group, group end tag not found!");
		return;
	}
	if (tag_error)
	{
		return;
	}
	if (current->parent == NULL)
	{
		TF_ERROR("Cannot leave group, at highest level already!");
		return;
	}
	current = current->parent->current; // resume at next tag
	if (current->element != tag)
	{
		TF_ERROR("Trying to leave the wrong group! We are in %d, leaving %d",
		         current->parent != NULL ? (int)current->parent->element : 0, (int)tag);
		return;
	}
	assert(current != NULL);
}

uint32_t tagRead(element_t tag)
{
	uint8_t tagtype;

	if (!scanforward(tag))
	{
		if (current->defaultval && current->element == tag)
		{
			return current->val.uint32_tval;
		}
		return 0;
	}

	if (!PHYSFS_readUBE8(handle, &tagtype))
	{
		TF_ERROR("tagread: Tag type not found: %d", (int)tag);
		tag_error = true;
		return 0;
	}
	if (tagtype == TF_INT_U32)
	{
		uint32_t val;
		(void) PHYSFS_readUBE32(handle, &val);
		return val;
	}
	else if (tagtype == TF_INT_U16)
	{
		uint16_t val;
		(void) PHYSFS_readUBE16(handle, &val);
		return val;
	}
	else if (tagtype == TF_INT_U8)
	{
		uint8_t val;
		(void) PHYSFS_readUBE8(handle, &val);
		return val;
	}
	else
	{
		TF_ERROR("readtag: Error reading tag %d, bad type", (int)tag);
		return 0;
	}
}

int32_t tagReads(element_t tag)
{
	uint8_t tagtype;

	if (!scanforward(tag))
	{
		if (current->defaultval && current->element == tag)
		{
			return current->val.int32_tval;
		}
		return 0;
	}

	if (!PHYSFS_readUBE8(handle, &tagtype))
	{
		TF_ERROR("tagreads: Tag type not found: %d", (int)tag);
		return 0;
	}
	if (tagtype == TF_INT_S32)
	{
		int32_t val;
		(void) PHYSFS_readSBE32(handle, &val);
		return val;
	}
	else if (tagtype == TF_INT_S16)
	{
		int16_t val;
		(void) PHYSFS_readSBE16(handle, &val);
		return val;
	}
	else if (tagtype == TF_INT_S8)
	{
		int8_t val;
		(void) PHYSFS_readSBE8(handle, &val);
		return val;
	}
	else
	{
		TF_ERROR("readtags: Error reading tag %d, bad type", (int)tag);
		return 0;
	}
}

float tagReadf(element_t tag)
{
	uint8_t tagtype;

	if (!scanforward(tag))
	{
		if (current->defaultval && current->element == tag)
		{
			return current->val.floatval;
		}
		return 0;
	}

	if (!PHYSFS_readUBE8(handle, &tagtype))
	{
		TF_ERROR("tagreadf: Tag type not found: %d", (int)tag);
		return 0;
	}
	if (tagtype == TF_INT_FLOAT)
	{
		float val;
		(void) PHYSFS_readBEFloat(handle, &val);
		return val;
	}
	else
	{
		TF_ERROR("readtagf: Error reading tag %d, bad type", (int)tag);
		return 0;
	}
}

bool tagReadBool(element_t tag)
{
	uint8_t tagtype;

	if (!scanforward(tag))
	{
		if (current->defaultval && current->element == tag)
		{
			return current->val.uint32_tval;
		}
		return 0;
	}

	if (!PHYSFS_readUBE8(handle, &tagtype))
	{
		TF_ERROR("tagReadBool: Tag type not found: %d", (int)tag);
		return 0;
	}
	if (tagtype == TF_INT_BOOL || tagtype == TF_INT_U8)
	{
		uint8_t val;
		(void) PHYSFS_readUBE8(handle, &val);
		return val;
	}
	else
	{
		TF_ERROR("readTagBool: Error reading tag %d, bad type", (int)tag);
		return 0;
	}
}

bool tagReadfv(element_t tag, uint16_t size, float *vals)
{
	uint8_t tagtype;
	uint16_t count;
	int i;

	if (!scanforward(tag))
	{
		return false;
	}
	if (!PHYSFS_readUBE8(handle, &tagtype) || tagtype != TF_INT_FLOAT_ARRAY)
	{
		TF_ERROR("tagreadfv: Tag type not found: %d", (int)tag);
		return false;
	}
	if (!PHYSFS_readUBE16(handle, &count) || count != size)
	{
		TF_ERROR("tagreadfv: Bad size: %d", (int)tag);
		return false;
	}
	for (i = 0; i < size; i++)
	{
		if (!PHYSFS_readBEFloat(handle, &vals[i]))
		{
			TF_ERROR("tagreadfv: Error reading idx %d, tag %d", i, (int)tag);
			return false;
		}
	}
	return true;
}

bool tagRead16v(element_t tag, uint16_t size, uint16_t *vals)
{
	uint8_t tagtype;
	uint16_t count;
	int i;

	if (!scanforward(tag))
	{
		return false;
	}
	if (!PHYSFS_readUBE8(handle, &tagtype) || tagtype != TF_INT_U16_ARRAY)
	{
		TF_ERROR("tagread16v: Tag type not found: %d", (int)tag);
		return false;
	}
	if (!PHYSFS_readUBE16(handle, &count) || count != size)
	{
		TF_ERROR("tagread16v: Bad size: %d", (int)tag);
		return false;
	}
	for (i = 0; i < size; i++)
	{
		if (!PHYSFS_readUBE16(handle, &vals[i]))
		{
			TF_ERROR("tagread16v: Error reading idx %d, tag %d", i, (int)tag);
			return false;
		}
	}
	return true;
}

bool tagReadString(element_t tag, uint16_t size, char *buffer)
{
	uint8_t tagtype = 255;
	PHYSFS_uint16 actualsize = 0;
	PHYSFS_sint64 readsize = 0;

	if (!scanforward(tag))
	{
		return false;
	}
	if (!PHYSFS_readUBE8(handle, &tagtype) || !PHYSFS_readUBE16(handle, &actualsize) || tagtype != TF_INT_U8_ARRAY)
	{
		TF_ERROR("Error reading string size: %s", PHYSFS_getLastError());
		return false;
	}
	if (actualsize > size)
	{
		TF_ERROR("String size %d larger than user maxlen %d", (int)actualsize, (int)size);
		return false;
	}
	readsize = PHYSFS_read(handle, buffer, 1, actualsize);
	if (readsize != actualsize)
	{
		TF_ERROR("Unfinished string: %s", PHYSFS_getLastError());
		return false;
	}
	// Return string MUST be zero terminated!
	if (*(buffer + actualsize - 1) != '\0')
	{
		TF_ERROR("Element %d is a string that is not zero terminated", (int)tag);
		return false;
	}
	return true;
}

char *tagReadStringDup(element_t tag)
{
	uint8_t tagtype;
	PHYSFS_uint16 actualsize;
	PHYSFS_sint64 readsize;
	char *buffer;

	if (!scanforward(tag))
	{
		return NULL;
	}
	if (!PHYSFS_readUBE8(handle, &tagtype) || !PHYSFS_readUBE16(handle, &actualsize))
	{
		TF_ERROR("tagread_stringdup: Error reading string size: %s", PHYSFS_getLastError());
		return NULL;
	}
	if (tagtype != TF_INT_U8_ARRAY)
	{
		TF_ERROR("tagread_stringdup: Bad string format");
		return NULL;
	}
	buffer = malloc(actualsize);
	assert(buffer != NULL);
	readsize = PHYSFS_read(handle, buffer, 1, actualsize);
	if (readsize != actualsize || *(buffer + actualsize - 1) != '\0')
	{
		TF_ERROR("tagread_stringdup: Unfinished or unterminated string: %s", PHYSFS_getLastError());
		free(buffer);
		return NULL;
	}
	return buffer;
}


/*********  TAGWRITE *********/


static bool write_tag(element_t tag)
{
	PHYSFS_sint64 size;

	if (tag_error || current == NULL || readmode)
	{
		return false;
	}
	size = PHYSFS_write(handle, &tag, 1, 1);
	if (size != 1)
	{
		TF_ERROR("Could not write tag: %s", PHYSFS_getLastError());
		return false;
	}
	return true;
}

bool tagWriteSeparator()
{
	if (tag_error || !write_tag(TAG_SEPARATOR))
	{
		return false;
	}
	if (current->parent == NULL)
	{
		current = first; // topmost group
	}
	else
	{
		current = current->parent->group; // reset to start of group tag index
	}
	// it has no payload
	return true;
}

bool tagWriteEnter(element_t tag, uint16_t elements)
{
	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (tag_error || !write_tag(tag) || !scan_to(tag))
	{
		return false;
	}
	assert(current->element == tag);
	ASSERT(current->vr[0] == 'G' && current->vr[1] == 'R', "Wrong type in writing %d", (int)tag);
	ASSERT(current->group != NULL, "Cannot write group, none defined for element %d!", tag);
	assert(current->group->parent != NULL);
	(void) PHYSFS_writeUBE8(handle, TF_INT_GROUP);
	(void) PHYSFS_writeUBE16(handle, elements);
	current->group->parent->current = current; // save where we are. ok, this is very contrived code.
	current = current->group;
	return true;
}

bool tagWriteLeave(element_t tag)
{
	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (tag_error || !write_tag(TAG_GROUP_END) || !scan_to(TAG_GROUP_END))
	{
		return false;
	}
	ASSERT(current->parent != NULL, "Cannot leave group %d, at highest level already!", (int)tag);
	if (current->parent == NULL)
	{
		TF_ERROR("Cannot leave group, at highest level already!");
		return false;
	}
	current = current->parent->current; // resume at next tag
	if (current->element != tag)
	{
		TF_ERROR("Trying to leave the wrong group! We are in %d, leaving %d",
		         current->parent != NULL ? (int)current->parent->element : 0, (int)tag);
		return false;
	}
	// it has no payload
	assert(current != NULL);
	return true;
}

bool tagWrite(element_t tag, uint32_t val)
{
	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (!scan_to(tag))
	{
		return false;
	}
	ASSERT(current->vr[0] == 'U' && current->vr[1] == 'S', "Wrong type in writing %d", (int)tag);
	if (current->defaultval && current->val.uint32_tval == val)
	{
		return true; // using default value to save disk space
	}
	if (!write_tag(tag))
	{
		return false;
	}
	if (val > UINT16_MAX)
	{
		(void) PHYSFS_writeUBE8(handle, TF_INT_U32);
		(void) PHYSFS_writeUBE32(handle, val);
	}
	else if (val > UINT8_MAX)
	{
		(void) PHYSFS_writeUBE8(handle, TF_INT_U16);
		(void) PHYSFS_writeUBE16(handle, val);
	}
	else
	{
		(void) PHYSFS_writeUBE8(handle, TF_INT_U8);
		(void) PHYSFS_writeUBE8(handle, val);
	}
	return true;
}

bool tagWrites(element_t tag, int32_t val)
{
	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (!scan_to(tag))
	{
		return false;
	}
	ASSERT(current->vr[0] == 'S' && current->vr[1] == 'I', "Wrong type in writing %d", (int)tag);
	if (current->defaultval && current->val.int32_tval == val)
	{
		return true; // using default value to save disk space
	}
	if (!write_tag(tag))
	{
		return false;
	}
	if (val > INT16_MAX)
	{
		(void) PHYSFS_writeUBE8(handle, TF_INT_S32);
		(void) PHYSFS_writeSBE32(handle, val);
	}
	else if (val > INT8_MAX)
	{
		(void) PHYSFS_writeUBE8(handle, TF_INT_S16);
		(void) PHYSFS_writeSBE16(handle, val);
	}
	else
	{
		(void) PHYSFS_writeUBE8(handle, TF_INT_S8);
		(void) PHYSFS_writeSBE8(handle, val);
	}
	return true;
}

bool tagWritef(element_t tag, float val)
{
	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (!scan_to(tag))
	{
		return false;
	}
	ASSERT(current->vr[0] == 'F' && current->vr[1] == 'P', "Wrong type in writing %d", (int)tag);
	if (current->defaultval && current->val.floatval == val)
	{
		return true; // using default value to save disk space
	}
	if (!write_tag(tag))
	{
		return false;
	}
	(void) PHYSFS_writeUBE8(handle, TF_INT_FLOAT);
	(void) PHYSFS_writeBEFloat(handle, val);
	return true;
}

bool tagWriteBool(element_t tag, bool val)
{
	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (!scan_to(tag))
	{
		return false;
	}
	ASSERT(current->vr[0] == 'B' && current->vr[1] == 'O', "Wrong type in writing %d", (int)tag);
	if (current->defaultval && current->val.uint32_tval == val)
	{
		return true; // using default value to save disk space
	}
	if (!write_tag(tag))
	{
		return false;
	}
	(void) PHYSFS_writeUBE8(handle, TF_INT_BOOL);
	(void) PHYSFS_writeUBE8(handle, val);
	return true;
}

bool tagWritefv(element_t tag, uint16_t count, float *vals)
{
	int i;

	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (!scan_to(tag) || !write_tag(tag))
	{
		return false;
	}
	ASSERT(current->vr[0] == 'F' && current->vr[1] == 'P', "Wrong type in writing %d", (int)tag);
	(void) PHYSFS_writeUBE8(handle, TF_INT_FLOAT_ARRAY);
	(void) PHYSFS_writeUBE16(handle, count);
	for (i = 0; i < count; i++)
	{
		(void) PHYSFS_writeBEFloat(handle, vals[i]);
	}
	return true;
}

bool tagWrite16v(element_t tag, uint16_t count, uint16_t *vals)
{
	int i;

	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (!scan_to(tag) || !write_tag(tag))
	{
		return false;
	}
	ASSERT(current->vr[0] == 'U' && current->vr[1] == 'S', "Wrong type in writing %d", (int)tag);
	(void) PHYSFS_writeUBE8(handle, TF_INT_U16_ARRAY);
	(void) PHYSFS_writeUBE16(handle, count);
	for (i = 0; i < count; i++)
	{
		(void) PHYSFS_writeUBE16(handle, vals[i]);
	}
	return true;
}

bool tagWriteString(element_t tag, const char *buffer)
{
	size_t size;

	assert(tag != TAG_SEPARATOR && tag != TAG_GROUP_END);
	if (!scan_to(tag) || !write_tag(tag))
	{
		return false;
	}
	ASSERT(current->vr[0] == 'S' && current->vr[1] == 'T', "Wrong type in writing %d", (int)tag);

	// find size of string
	size = strlen(buffer) + 1;
	if (size > current->vm)
	{
		TF_ERROR("Given string is too long (size %d > limit %d)", (int)size, (int)current->vm);
		return false;
	}
	(void) PHYSFS_writeUBE8(handle, TF_INT_U8_ARRAY);
	(void) PHYSFS_writeUBE16(handle, size);
	(void) PHYSFS_write(handle, buffer, 1, size);
	return true;
}


/*********  TAGFILE UNIT TEST *********/


void tagTest()
{
	const char *writename = "test.wzs";
	const char *cformat = "WZTAGFILE1";
	char format[300], *formatdup;
	uint16_t droidpos[3];
	float fv[3];

	tagOpenWrite("testdata/tagfile_virtual.def", writename);
	tagWrites(0x05, 11);
	tagWriteString(0x06, cformat);
	ASSERT(!tagGetError(), "Error 1: %s", tagGetErrorString());
	tagClose();

	tagOpenRead("testdata/tagfile_virtual.def", "test.wzs");
	assert(tagRead(0x01) == 1);
	assert(tagReads(0x02) == 2);
	assert(tagReadf(0x03) == 3);
	assert(tagRead(0x04) == 4);
	assert(tagReads(0x05) == 11);
	formatdup = tagReadStringDup(0x06);
	assert(strncmp(formatdup, cformat, 9) == 0);
	free(formatdup);
	assert(tagReads(0x07) == 1);
	ASSERT(!tagGetError(), "Error 2: %s", tagGetErrorString());
	tagClose();

	tagOpenWrite("testdata/tagfile_basic.def", writename);
	ASSERT(!tagGetError(), "Error 3: %s", tagGetErrorString());
	tagWriteString(0x01, cformat);
	tagWriteEnter(0x02, 1);
		tagWrite(0x01, 101);
		droidpos[0] = 11;
		droidpos[1] = 13;
		droidpos[2] = 15;
		tagWrite16v(0x02, 3, droidpos);
		fv[0] = 0.1f;
		fv[1] = 1.1f;
		fv[2] = -1.3f;
		tagWritefv(0x03, 3, fv);
		tagWriteEnter(0x09, 1);
			tagWrite(0x01, 1);
		tagWriteLeave(0x09);
	tagWriteLeave(0x02);
	tagClose();

	memset(droidpos, 0, 6);
	memset(fv, 0, 6);
	tagOpenRead("testdata/tagfile_basic.def", writename);
	tagReadString(0x01, 200, format);
	assert(strncmp(format, cformat, 9) == 0);
	tagReadEnter(0x02);
		assert(tagRead(0x01) == 101);
		tagRead16v(0x02, 3, droidpos);
		assert(droidpos[0] == 11);
		assert(droidpos[1] == 13);
		assert(droidpos[2] == 15);
		tagReadfv(0x03, 3, fv);
		assert(fv[0] - 0.1f < 0.001);
		assert(fv[1] - 1.1f < 0.001);
		assert(fv[2] + 1.3f < 0.001);
	tagReadLeave(0x02);
	assert(tagRead(0x04) == 9);
	ASSERT(!tagGetError(), "Error 4: %s", tagGetErrorString());
	tagClose();
}
