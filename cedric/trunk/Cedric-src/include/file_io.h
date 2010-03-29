#ifdef __cplusplus
 extern "C" {
#endif
#ifndef FILEIO_WAS_INCLUDED
#define FILEIO_WAS_INCLUDED

#include <stdio.h>
#include <sys/types.h>

typedef struct stat stat_struct_t; 

/* FILE_IO - cover for file io routines. */

/*********************************************************
 * filecopy() - Utility routine to copy the contents of one
 *              file to another file.
 *
 * Returns -1 on error, 0 otherwise.
 */

extern int filecopy(FILE *dest, FILE *source);


/********************************************************
 * makedir()
 *
 * Utility routine to create a directory.  If the directory
 * already exists, does nothing. Uses ta_makedir().
 *
 * Returns -1 on error, 0 otherwise.
 */

extern int makedir(char *path);

/********************************************************
 * ta_makedir()
 *
 * Utility routine to create a directory.  If the directory
 * already exists, does nothing.
 *
 * Returns -1 on error, 0 otherwise.
 */

extern int ta_makedir(const char *path);

/********************************************************
 * ta_makedir_recurse()
 *
 * Utility routine to create a directory recursively.
 * If the directory already exists, does nothing.
 * Otherwise it recurses through the path, making all
 * needed directories.
 *
 * Returns -1 on error, 0 otherwise.
 */
extern int ta_makedir_recurse(const char *path);

/********************************************************
 * ta_fread()
 *
 * Wrapper for fread() - takes care of interrupted read.
 * Returns same as fread()
 *
 */

extern int ta_fread(char *ptr, int size, int nitems, FILE *stream);

/*********************************************************
 * ta_fwrite()
 *
 * Wrapper for fwrite() - takes care of interrupted write.
 * Returns same as fwrite()
 */

extern int ta_fwrite(char *ptr, int size, int nitems, FILE *stream);

/*******************************************************
 *
 * ta_file_uncompress()
 *
 * Uncompresses file if it is compressed and the uncompressed file
 * doesn't already exist.
 * Handles compress and gzip type files.
 * Returns 1 if uncompression done, 0 if not, -1 if error
 * If file_path has .Z or .gz extension, the extension is
 * removed before the function returns.
 */

extern int ta_file_uncompress(char *file_path);

/*********************************************************
 * ta_fopen_uncompress()
 *
 * Uncompresses the file if necessary, then opens it
 *
 * Return is identical to fopen()
 */

extern FILE *ta_fopen_uncompress(char *filename, char *type);

/*********************************************************
 * ta_stat_uncompress()
 *
 * stats a file in uncompressed or compressed state.
 *
 * Return is identical to stat()
 */

extern int ta_stat_uncompress(char *path, stat_struct_t *buf);

/*********************************************
 * ta_lock_file()
 *
 * Sets up read or write lock on entire file.
 *
 * File must already be open - path is passed in for
 * error reporting only.
 *
 * Type is "r" or "w", for read or write lock respectively.
 *
 * Blocks until lock is obtained.
 *
 * Returns 0 on success, -1 on failure.
 */

extern int ta_lock_file(const char *file_path, FILE *fd,
			const char *type);

/*********************************************
 * ta_lock_file_procmap()
 *
 * Sets up read or write lock on entire file.
 * Loops waiting for lock. Reports to procmap while
 * waiting.
 *
 * File must already be open - path is passed in for
 * error reporting only.
 *
 * Type is "r" or "w", for read or write lock respectively.
 *
 * Returns 0 on success, -1 on failure.
 */

extern int ta_lock_file_procmap(const char *file_path, FILE *fd,
				const char *type);

/*********************************************
 * ta_unlock_file()
 *
 * Clear lock on entire file.
 *
 * File must already be open - path is passed in for
 * error reporting only.
 *
 * Returns 0 on success, -1 on failure.
 */

extern int ta_unlock_file(const char *file_path, FILE *fd);

/*********************************************
 * ta_create_lock_file()
 *
 * Creates a lock file with the given path.
 * If the file does not exist it is created and opened.
 * If it exists, it is opened.
 * After opening it is write locked.
 *
 * Returns FILE pointer on success, NULL on failure.
 * Failure occurs either because the file cannot be
 * opened, or because the file is already write-locked.
 */

extern FILE *ta_create_lock_file(const char *lock_file_path);

/*********************************************
 * ta_remove_lock_file()
 *
 * Unlocks the file pointer and closes it.
 * Removes the lock file with the given path.
 *
 * Returns 0 on success, -1 on failure.
 */

extern int ta_remove_lock_file(const char *lock_file_path, FILE *fd);

/*********************************************************************
 * ta_tmp_path_from_dir()
 *
 * Given a final file dir, fills a string with a
 * temporary file path.
 *
 * The intended use is to provide a tmp file path to which a file
 * is written prior to renaming to the final name.
 *
 * The tmp path is in the same directory as the final path.
 *
 * If tmp_file_name is non-null, it is used for the file name.
 * If it is NULL, the name is 'tmp.pid.tmp', where pid is
 * determined using the getpid() function.
 *
 * Memory for the string tmp_file_path must be allocated by the calling
 * routine to be at least max_path_len long.
 *
 */

extern void ta_tmp_path_from_dir(char *final_file_dir,
				 char *tmp_file_path,
				 int max_path_len,
				 char *tmp_file_name);

/*********************************************************************
 * ta_tmp_path_from_final()
 *
 * Given a final file path, fills a string with a
 * temporary file path.
 *
 * The intended use is to provide a tmp file path to which a file
 * is written prior to renaming to the final name.
 *
 * The tmp path is in the same directory as the final path.
 *
 * If tmp_file_name is non-null, it is used for the file name.
 * If it is NULL, the name is 'tmp.pid.tmp', where pid is
 * determined using the getpid() function.
 *
 * Memory for the string tmp_file_path must be allocated by the calling
 * routine to be at least max_path_len long.
 *
 */

extern void ta_tmp_path_from_final(char *final_file_path,
				   char *tmp_file_path,
				   int max_path_len,
				   char *tmp_file_name);

/******************************************************
 * ta_read_select - waits for read access on a file ptr
 *
 * returns 1 on success, -1 on timeout, -2 on failure
 *
 * Blocks if wait_msecs == -1
 */

extern int ta_read_select(FILE *fp, long wait_msecs);

/******************************************************
 * ta_stat - wrapper for stat()
 *
 * Same functionality as stat(). This function is used in
 * some places because some implementations of stat() use
 * inline functions. This can cause problems in linking
 * C executable with libraries such as toolsa which mix
 * C and C++ code.
 */

extern int ta_stat(const char *file_name, struct stat *buf);

/******************************************************
 * ta_fstat - wrapper for fstat()
 *
 * Same functionality as fstat(). This function is used in
 * some places because some implementations of fstat() use
 * inline functions. This can cause problems in linking
 * C executable with libraries such as toolsa which mix
 * C and C++ code.
 */

extern int ta_fstat(int filedes, struct stat *buf);

#endif
#ifdef __cplusplus
}
#endif


