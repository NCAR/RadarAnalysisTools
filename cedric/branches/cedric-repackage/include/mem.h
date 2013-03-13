#ifdef __cplusplus
 extern "C" {
#endif

#ifndef MEM_WAS_INCLUDED
#define MEM_WAS_INCLUDED

#include <sys/types.h>
#include <memory.h>

#define MEM_zero(a) memset(&(a), 0, sizeof((a)))

extern void *MEM_alloc(size_t size);
extern void *MEM_calloc (size_t nelem, size_t elsize);
extern void *MEM_realloc (void *ptr, size_t size);
extern void MEM_free (void *ptr);

/*
 * umalloc
 */

#include <malloc.h>

extern void *umalloc(size_t size);
extern void *ucalloc(size_t num, size_t size);
extern void *urealloc(void *ptr, size_t size);
extern void ufree(void *ptr);
extern void ufree_non_null(void **ptr_p);

extern void umalloc_debug(int level);
extern void **umalloc2(size_t m, size_t n, size_t item_size);
extern void ***umalloc3(size_t l, size_t m, size_t n, size_t item_size);
extern void **ucalloc2(size_t m, size_t n, size_t item_size);
extern void ***ucalloc3(size_t l, size_t m, size_t n, size_t item_size);
extern void ufree2(void **two_d_pointers);
extern void ufree3(void ***three_d_pointers);
extern void umalloc_map(void);
extern int umalloc_count(void);
extern void umalloc_verify(void);


#endif /* MEM_WAS_INCLUDED */

#ifdef __cplusplus
}
#endif


