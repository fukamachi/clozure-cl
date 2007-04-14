/*
   Copyright (C) 1994-2001 Digitool, Inc
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include <limits.h>
#undef USE_MACH_SEMAPHORES
#undef USE_POSIX_SEMAPHORES
#ifdef DARWIN
#define USE_MACH_SEMAPHORES 1
#endif
#ifndef USE_MACH_SEMAPHORES
#define USE_POSIX_SEMAPHORES
#include <semaphore.h>
#endif
#ifdef USE_MACH_SEMAPHORES
/* We have to use Mach semaphores, even if we're otherwise 
   using POSIX signals, etc. */
#include <mach/task.h>
#include <mach/semaphore.h>
#endif
#include <limits.h>
#ifdef FREEBSD
#include <pthread_np.h>
#endif
#include <sched.h>

#include "lisp.h"
#include "lisp_globals.h"
#include "gc.h"

Boolean extern threads_initialized;

#define TCR_TO_TSD(tcr) ((void *)((natural)(tcr)+TCR_BIAS))
#define TCR_FROM_TSD(tsd) ((TCR *)((natural)(tsd)-TCR_BIAS))

#ifdef USE_POSIX_SEMAPHORES
typedef sem_t * SEMAPHORE;
#define SEM_WAIT(s) sem_wait((SEMAPHORE)s)
#define SEM_RAISE(s) sem_post((SEMAPHORE)s)
#define SEM_TIMEDWAIT(s,t) sem_timedwait((SEMAPHORE)s,(struct timespec *)t)
#endif

#ifdef USE_MACH_SEMAPHORES
typedef semaphore_t SEMAPHORE;
#define SEM_WAIT(s) semaphore_wait((SEMAPHORE)(natural)s)
#define SEM_RAISE(s) semaphore_signal((SEMAPHORE)(natural)s)
#define SEM_TIMEDWAIT(s,t) semaphore_timedwait((SEMAPHORE)(natural)s,t)
#endif

void sem_wait_forever(SEMAPHORE s);

#ifdef USE_POSIX_SEMAPHORES
#define SEM_WAIT_FOREVER(s) sem_wait_forever((SEMAPHORE)s)
#endif

#ifdef USE_MACH_SEMAPHORES
#define SEM_WAIT_FOREVER(s) sem_wait_forever((SEMAPHORE)(natural)s)
#endif

typedef struct
{
  signed_natural avail;
  TCR* owner;
  signed_natural  count;
  void* signal;
  signed_natural waiting;
  void *malloced_ptr;
  signed_natural spinlock;
} _recursive_lock, *RECURSIVE_LOCK;


int lock_recursive_lock(RECURSIVE_LOCK, TCR *);
int unlock_recursive_lock(RECURSIVE_LOCK, TCR *);
RECURSIVE_LOCK new_recursive_lock(void);
void destroy_recursive_lock(RECURSIVE_LOCK);
int recursive_lock_trylock(RECURSIVE_LOCK, TCR *, int *);

#define LOCK(m, t) lock_recursive_lock((RECURSIVE_LOCK)ptr_from_lispobj(m), (TCR *)t)
#define UNLOCK(m, t) unlock_recursive_lock((RECURSIVE_LOCK)ptr_from_lispobj(m), (TCR *)t)

/* Hmm.  This doesn't look like the MacOS Thread Manager ... */
LispObj current_thread_osid(void);
void *current_native_thread_id(void);
void *new_semaphore(int);
void destroy_semaphore(void**);
void tsd_set(LispObj, void *);
void *tsd_get(LispObj);
TCR *new_tcr(natural, natural);
TCR *initial_thread_tcr;

#define DEFAULT_THREAD_STACK_SIZE ((size_t) -1)
#define MINIMAL_THREAD_STACK_SIZE ((size_t) 0)


LispObj create_system_thread(size_t stack_size, 
			     void* stackaddr,
			     void* (*start_routine)(void *),
			     void* param);

TCR *get_tcr(Boolean);
TCR *get_interrupt_tcr(Boolean);
Boolean suspend_tcr(TCR *);
Boolean resume_tcr(TCR *);

typedef struct _rwquentry 
{
  struct _rwquentry *prev;
  struct _rwquentry *next;
  TCR *tcr;
  int count;
} rwquentry;

typedef struct
{
  rwquentry head;
  int state;                    /* sum of all counts on queue */
  pthread_mutex_t *lock;        /* lock access to this data structure */
  pthread_cond_t *reader_signal;
  pthread_cond_t *writer_signal;
  int write_wait_count;
  int read_wait_count;
  int dying;
  rwquentry freelist;
} rwlock;

#define RWLOCK_WRITER(rw) rw->head.tcr
#define RWLOCK_WRITE_COUNT(rw) rw->head.count

rwlock * rwlock_new(void);
int rwlock_destroy(rwlock *);
int rwlock_rlock(rwlock *, TCR *, struct timespec *);
int rwlock_wlock(rwlock *, TCR *, struct timespec *);
int rwlock_try_wlock(rwlock *, TCR *);
int rwlock_unlock(rwlock *, TCR *);


natural 
atomic_and(natural*, natural);

natural 
atomic_ior(natural*, natural);

#define SET_TCR_FLAG(t,bit) atomic_ior(&(t->flags),(1L<<bit))
#define CLR_TCR_FLAG(t,bit) atomic_and(&(t->flags),~(1L<<bit))


#ifdef SIGRTMIN
#define SIG_SUSPEND_THREAD (SIGRTMIN+6)
#define SIG_RESUME_THREAD (SIG_SUSPEND_THREAD+1)
#else
#define SIG_SUSPEND_THREAD SIGUSR1
#define SIG_RESUME_THREAD SIGUSR2
#endif

extern int thread_suspend_signal, thread_resume_signal;

void
suspend_resume_handler(int, siginfo_t *, ExceptionInformation *);

/* Maybe later
Boolean
rwlock_try_rlock(rwlock *);

Boolean
rwlock_try_wlock(rwlock *);
*/
