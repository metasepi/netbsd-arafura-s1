#include "rts/conc.h"

void
jhc_mutex_init(jhc_mutex_t *mutex)
{
	mutex_init(mutex, MUTEX_DEFAULT, IPL_HIGH);
}

void
jhc_mutex_lock(jhc_mutex_t *mutex)
{
	mutex_enter(mutex);
}

void
jhc_mutex_unlock(jhc_mutex_t *mutex)
{
	mutex_exit(mutex);
}

jhc_threadid_t
forkOS_createThread(void *(*wrapper) (void *), void *entry, int *err)
{
	panic("forkOS_createThread: not yet impl");
	/* NOTREACHED */
	return 1;
}
