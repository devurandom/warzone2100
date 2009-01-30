/*
	Copyright (c) 2008  Giel van Schijndel

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	1. The origin of this software must not be misrepresented; you must not
	   claim that you wrote the original software. If you use this software
	   in a product, an acknowledgment in the product documentation would be
	   appreciated but is not required.

	2. Altered source versions must be plainly marked as such, and must not be
	   misrepresented as being the original software.

	3. This notice may not be removed or altered from any source
	   distribution.
*/

/* These are necessary for the `bool' type and `strlcpy'. Allow frame header
 * files to be singly included */
#define FRAME_LIB_INCLUDE
#include "lib/framework/strlfuncs.h"
#include "lib/framework/types.h"

#include "physfs_vfs.h"
#include <sqlite3.h>
#include <physfs.h>
#include <assert.h>

typedef struct physfs_sqlite3_file physfs_sqlite3_file;

/** "Subclassed" sqlite3_file for this PhysicsFS VFS.
 */
struct physfs_sqlite3_file
{
	sqlite3_file SQLite3_file;
	PHYSFS_file* file;
	bool NOOP;                      /**< Flag which indicates whether all write operations should be no-ops (and reads should fail) */
};

/** Closes the given file and deallocates any associated resources.
 *  \param f the physfs_sqlite3_file to close
 *  \return SQLITE_OK on success.
 */
static int xClose(sqlite3_file* f)
{
	physfs_sqlite3_file * const file = (physfs_sqlite3_file * const)f;
	assert(&file->SQLite3_file == f);

	if (file->NOOP)
		return SQLITE_OK;

	if (file->file)
		PHYSFS_close(file->file);

	return SQLITE_OK;
}

/** Reads bytes from the file into the given output buffer.
 *  \param f        the physfs_sqlite3_file to read from.
 *  \param[out] dst the memory buffer to write to.
 *  \param iAmt     the amount of bytes to read from the file.
 *  \param iOfst    the (absolute) offset into the file to start reading from.
 *  \return SQLITE_OK on success, SQLITE_IOERR_SHORT_READ if we couldn't read
 *          the full amount of \c iAmt bytes because EOF was reached, or
 *          SQLITE_IOERR_READ for any other error.
 */
static int xRead(sqlite3_file* f, void* dst, int iAmt, sqlite3_int64 iOfst)
{
	PHYSFS_sint64 nRead;

	physfs_sqlite3_file * const file = (physfs_sqlite3_file * const)f;
	assert(&file->SQLite3_file == f);

	if (file->NOOP)
		return SQLITE_IOERR;

	/* Seek to the given offset */
	if (!PHYSFS_seek(file->file, iOfst))
		return SQLITE_IOERR_READ;

	nRead = PHYSFS_read(file->file, dst, 1, iAmt);

	if (nRead < 0)
		return SQLITE_IOERR_READ;
	else if (nRead < iAmt)
		return SQLITE_IOERR_SHORT_READ;
	else
		return SQLITE_OK;
}

/** Dummy write implementation (doesn't do anything at all).
 *  \return SQLITE_IOERR_WRITE
 */
static int xWrite(sqlite3_file* f, WZ_DECL_UNUSED const void* src, WZ_DECL_UNUSED int iAmt, WZ_DECL_UNUSED sqlite3_int64 iOfst)
{
	physfs_sqlite3_file * const file = (physfs_sqlite3_file * const)f;
	assert(&file->SQLite3_file == f);

	if (file->NOOP)
		return SQLITE_OK;

	return SQLITE_IOERR_WRITE;
}

/** Dummy truncate implementation (doesn't do anything at all).
 *  \return SQLITE_IOERR_TRUNCATE
 */
static int xTruncate(sqlite3_file* f, WZ_DECL_UNUSED sqlite3_int64 size)
{
	physfs_sqlite3_file * const file = (physfs_sqlite3_file * const)f;
	assert(&file->SQLite3_file == f);

	if (file->NOOP)
		return SQLITE_OK;

	return SQLITE_IOERR_TRUNCATE;
}

/** Dummy sync implementation (doesn't do anything at all).
 *  \return SQLITE_IOERR_FSYNC
 */
static int xSync(sqlite3_file* f, WZ_DECL_UNUSED int flags)
{
	physfs_sqlite3_file * const file = (physfs_sqlite3_file * const)f;
	assert(&file->SQLite3_file == f);

	if (file->NOOP)
		return SQLITE_OK;

	return SQLITE_IOERR_FSYNC;
}

/** Determines the size of the given file.
 *  \param f          the physfs_sqlite3_file to determine the size of.
 *  \param[out] pSize to write the size of this file to.
 *  \return 
 */
static int xFileSize(sqlite3_file* f, sqlite3_int64* pSize)
{
	PHYSFS_sint64 size;

	physfs_sqlite3_file * const file = (physfs_sqlite3_file * const)f;
	assert(&file->SQLite3_file == f);

	if (file->NOOP)
		return SQLITE_IOERR;

	size = PHYSFS_fileLength(file->file);

	if (size < 0)
		return SQLITE_IOERR_FSTAT;

	*pSize = size;

	return SQLITE_OK;
}

static int xLock(WZ_DECL_UNUSED sqlite3_file* f, int level)
{
	switch (level)
	{
		case SQLITE_LOCK_NONE:
		case SQLITE_LOCK_SHARED:
			return SQLITE_OK;

		case SQLITE_LOCK_RESERVED:
		case SQLITE_LOCK_PENDING:
		case SQLITE_LOCK_EXCLUSIVE:
		default:
			return SQLITE_IOERR_RDLOCK;
	}
}

static int xUnlock(WZ_DECL_UNUSED sqlite3_file* f, WZ_DECL_UNUSED int level)
{
	return SQLITE_OK;
}

static int xCheckReservedLock(WZ_DECL_UNUSED sqlite3_file* f, int *pResOut)
{
	*pResOut = false;
	return SQLITE_OK;
}

static int xFileControl(WZ_DECL_UNUSED sqlite3_file* f, WZ_DECL_UNUSED int op, WZ_DECL_UNUSED void *pArg)
{
	return 0;
}

static int xSectorSize(WZ_DECL_UNUSED sqlite3_file* f)
{
	return 512;
}

static int xDeviceCharacteristics(WZ_DECL_UNUSED sqlite3_file* f)
{
	return 0;
}

static const sqlite3_io_methods physfs_sqlite3_io_methods =
{
	1,                      /**< iVersion */
	xClose,                 /**< Closes the physfs_sqlite3_file */
	xRead,                  /**< Reads from the physfs_sqlite3_file */
	xWrite,                 /**< Intended to write from the physfs_sqlite3_file */
	xTruncate,              /**< Intended to truncate the physfs_sqlite3_file */
	xSync,                  /**< Intended to flush data out of OS cache into non-volatile memory */
	xFileSize,              /**< Deterimines the physfs_sqlite3_file's size */
	xLock,                  /**< Acquires a lock on the physfs_sqlite3_file */
	xUnlock,                /**< Releases a lock on the physfs_sqlite3_file */
	xCheckReservedLock,     /**< Checks whether a "reserved" (or higher) level lock is acquired on this physfs_sqlite3_file */
	xFileControl,           /**< Intended to execute the given opcode */
	xSectorSize,            /**< Determines the size of sectors for the underlying filesystem */
	xDeviceCharacteristics, /**< Returns a bitmask describing the underlying filesystem's characteristics */
};

static int xOpen(WZ_DECL_UNUSED sqlite3_vfs* pVfs, const char* zName, sqlite3_file* f, int flags, int* pOutFlags)
{
	physfs_sqlite3_file * const file = (physfs_sqlite3_file * const)f;
	assert(&file->SQLite3_file == f);

	/* Assign the "overloaded" I/O functions to this file object */
	file->SQLite3_file.pMethods = &physfs_sqlite3_io_methods;

	/* We don't support journals in this implementation */
	if (flags & SQLITE_OPEN_MAIN_JOURNAL
	 || flags & SQLITE_OPEN_TEMP_JOURNAL
	 || flags & SQLITE_OPEN_MASTER_JOURNAL
	 || flags & SQLITE_OPEN_SUBJOURNAL)
	{
		file->file = 0;
		file->NOOP = true;
	}
	else /* if (flags & SQLITE_OPEN_MAIN_DB
	      || flags & SQLITE_OPEN_TEMP_DB
	      || flags & SQLITE_OPEN_TRANSIENT_DB) */
	{
		/* SQLite requires that we open a temporary file if zName is
		 * NULL, but we currently do not provide an implementation for
		 * that, so bail out.
		 */
		if (zName == NULL)
			return SQLITE_IOERR;

		file->file = PHYSFS_openRead(zName);
		file->NOOP = false;

		if (!file->file)
			return SQLITE_IOERR;
	}

	*pOutFlags = SQLITE_OPEN_READONLY;

	return SQLITE_OK;
}

static int xDelete(WZ_DECL_UNUSED sqlite3_vfs* pVfs, const char* zName, WZ_DECL_UNUSED int syncDir)
{
	if (PHYSFS_delete(zName))
		return SQLITE_OK;
	else
		return SQLITE_IOERR_DELETE;
}

static int xAccess(WZ_DECL_UNUSED sqlite3_vfs* pVfs, const char* zName, int flags, int *pResOut)
{
	switch (flags)
	{
		case SQLITE_ACCESS_EXISTS:
			*pResOut = PHYSFS_exists(zName);
			break;

		case SQLITE_ACCESS_READ:
		{
			PHYSFS_file* f = PHYSFS_openRead(zName);

			*pResOut = (f != NULL);

			PHYSFS_close(f);
			break;
		}
		case SQLITE_ACCESS_READWRITE:
		default:
			*pResOut = 0;
	}

	return SQLITE_OK;
}

/** \return non-zero when no truncation occurred, zero otherwise.
 */
static int xFullPathname(WZ_DECL_UNUSED sqlite3_vfs* pVfs, const char* zName, int nOut, char* zOut)
{
	/* nOut isn't ever negative, because its a buffer length, and the
	 * comparison below would cause a signed/unsigned comparison warning.
	 */
	const unsigned int _nOut = nOut;
	assert(nOut > 0);

	return (strlcpy(zOut, zName, _nOut) < _nOut) ? SQLITE_OK : SQLITE_IOERR;
}

static void* xDlOpen(WZ_DECL_UNUSED sqlite3_vfs* pVfs, WZ_DECL_UNUSED const char* zFilename)
{
	return NULL;
}

static void xDlError(WZ_DECL_UNUSED sqlite3_vfs* pVfs, int nByte, char* zErrMsg)
{
	strlcpy(zErrMsg, "DlOpen and DlSym API isn't supported for this (PhysicsFS \"physfs\") VFS.", nByte);
}

static void (*xDlSym(WZ_DECL_UNUSED sqlite3_vfs* pVfs, WZ_DECL_UNUSED void* dl, WZ_DECL_UNUSED const char* zSymbol))(void)
{
	return NULL;
}

static void xDlClose(WZ_DECL_UNUSED sqlite3_vfs* pVfs, WZ_DECL_UNUSED void* dl)
{
}

static int xRandomness(WZ_DECL_UNUSED sqlite3_vfs* pVfs, WZ_DECL_UNUSED int nByte, WZ_DECL_UNUSED char* zOut)
{
	return SQLITE_IOERR;
}

static int xSleep(WZ_DECL_UNUSED sqlite3_vfs* pVfs, WZ_DECL_UNUSED int microseconds)
{
	return SQLITE_IOERR;
}

static int xCurrentTime(WZ_DECL_UNUSED sqlite3_vfs* pVfs, WZ_DECL_UNUSED double* curTime)
{
	return SQLITE_IOERR;
}

static int xGetLastError(WZ_DECL_UNUSED sqlite3_vfs* pVfs, int nBuf, char* zBuf)
{
	/* nBuf isn't ever negative, because its a buffer length, and the
	 * comparison below would cause a signed/unsigned comparison warning.
	 */
	const unsigned int _nBuf = nBuf;
	assert(nBuf > 0);

	return (strlcpy(zBuf, PHYSFS_getLastError(), _nBuf) < _nBuf) ? SQLITE_OK : SQLITE_IOERR;
}

static sqlite3_vfs physfs_sqlite3_vfs =
{
	1,                              /**< Structure version number */
	sizeof(physfs_sqlite3_vfs),     /**< Size of subclassed sqlite3_file */
	256,                            /**< Maximum file pathname length */
	0,                              /**< Next registered VFS (managed by SQLite) */
	"physfs",                       /**< Name of this virtual file system */
	0,                              /**< Pointer to application-specific data */
	xOpen,
	xDelete,
	xAccess,
	xFullPathname,
	xDlOpen,
	xDlError,
	xDlSym,
	xDlClose,
	xRandomness,
	xSleep,
	xCurrentTime,
	xGetLastError,
};

void sqlite3_register_physfs_vfs(int makeDefault)
{
	sqlite3_vfs_register(&physfs_sqlite3_vfs, makeDefault);
}

void sqlite3_unregister_physfs_vfs()
{
	sqlite3_vfs_unregister(&physfs_sqlite3_vfs);
}
