#ifndef CONC_CUSTOM_H
#define CONC_CUSTOM_H
#include "jhc_rts_header.h"
#include <sys/mutex.h>

#define jhc_threadid_t          int // xxx dummy
#define jhc_mutex_t             kmutex_t

#endif /* CONC_CUSTOM_H */
