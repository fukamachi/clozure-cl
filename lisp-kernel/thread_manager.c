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


#include "Threads.h"

typedef struct {
  TCR *tcr;
  void *created;
} thread_activation;


extern void*
store_conditional(void**, void*, void*);

extern int
atomic_swap(int*, int);

int
atomic_incf_by(int *ptr, int by)
{
  int old, new;
  do {
    old = *ptr;
    new = old+by;
  } while (store_conditional((void **)ptr, (void *) old, (void *) new) !=
           (void *) old);
  return new;
}

int
atomic_incf(int *ptr)
{
  return atomic_incf_by(ptr, 1);
}

int
atomic_decf(int *ptr)
{
  int old, new;
  do {
    old = *ptr;
    new = old == 0 ? old : old-1;
  } while (store_conditional((void **)ptr, (void *) old, (void *) new) !=
           (void *) old);
  return old-1;
}


int
lock_recursive_lock(RECURSIVE_LOCK m, TCR *tcr, struct timespec *waitfor)
{

  if (tcr == NULL) {
    tcr = get_tcr(true);
  }
  if (m->owner == tcr) {
    m->count++;
    return 0;
  }
  while (1) {
    if (atomic_incf(&m->avail) == 1) {
      m->owner = tcr;
      m->count = 1;
      break;
    }
    SEM_WAIT(m->signal);
  }
  return 0;
}
  
int
unlock_recursive_lock(RECURSIVE_LOCK m, TCR *tcr)
{
  int ret = EPERM, pending;

  if (tcr == NULL) {
    tcr = get_tcr(true);
  }

  if (m->owner == tcr) {
    --m->count;
    if (m->count == 0) {
      m->owner = NULL;
      pending = atomic_swap(&m->avail, 0) - 1;
      atomic_incf_by(&m->waiting, pending);
      /* We're counting on atomic_decf not actually decrementing
	 the location below 0, but returning a negative result
	 in that case.
      */
      if (atomic_decf(&m->waiting) >= 0) {
	SEM_RAISE(m->signal);
      }
      ret = 0;
    }
  }
  return ret;
}

void
destroy_recursive_lock(RECURSIVE_LOCK m)
{
  destroy_semaphore((void **)&m->signal);
  free(m->malloced_ptr);
}

/*
  If we're already the owner (or if the lock is free), lock it
  and increment the lock count; otherwise, return EBUSY without
  waiting.
*/

int
recursive_lock_trylock(RECURSIVE_LOCK m, TCR *tcr, int *was_free)
{
  TCR *owner = m->owner;

  if (owner == tcr) {
    m->count++;
    if (was_free) {
      *was_free = 0;
      return 0;
    }
  }
  if (store_conditional((void **)&(m->avail), (void *)0, (void *)1) == (void *)0) {
    m->owner = tcr;
    m->count = 1;
    if (was_free) {
      *was_free = 1;
    }
    return 0;
  }

  return EBUSY;
}

  
int
wait_on_semaphore(SEMAPHORE s, int seconds, int nanos)
{
#ifdef LINUX
  struct timespec q;
  gettimeofday((struct timeval *)&q, NULL);
  q.tv_nsec *= 1000;

  q.tv_nsec += nanos;
  if (q.tv_nsec >= 1000000000) {
    q.tv_nsec -= 1000000000;
    seconds += 1;
  }
  q.tv_sec += seconds;
  return SEM_TIMEDWAIT(s, &q);
#endif
#ifdef DARWIN
  mach_timespec_t q = {seconds, nanos};
  return SEM_TIMEDWAIT(s, q);
#endif
}

void
signal_semaphore(SEMAPHORE s)
{
  SEM_RAISE(s);
}

  
LispObj
current_thread_osid()
{
  return (LispObj)pthread_self();
}

#ifdef SIGRTMIN
#define SIG_SUSPEND_THREAD (SIGRTMIN+6)
#define SIG_RESUME_THREAD (SIG_SUSPEND_THREAD+1)
#else
#define SIG_SUSPEND_THREAD SIGUSR1
#define SIG_RESUME_THREAD SIGUSR2
#endif


int thread_suspend_signal, thread_resume_signal;



void
linux_exception_init(TCR *tcr)
{
}


TCR *
get_interrupt_tcr()
{
#ifndef LINUX
  return get_tcr(true);
#else
  void* callers_r2 = current_r2;

  if (callers_r2 == NULL) {	/* pre-glibc-2.3.2 Linux */
    return get_tcr(true);
  } else {
    TCR  *head = (TCR *)lisp_global(INITIAL_TCR), *current = head;

    /* We can fairly safely assume that r2 contains either the current
       tcr or the current (linux) pthread structure, but we don't know
       which.  We can't lock anything or call any pthreads function until
       we're sure that r2 contains the current pthread pointer.

       We can identify r2 as a TCR if we find it in the global tcr list.
       Entries are only ever removed from the list when all threads other
       than the GC thread are suspended; additions keep the forward
       link (through tcr->next) consistent, so this traversal is safe.
    
  */
    do {
      if (current == callers_r2) {
	/* r2 contained the tcr.  Set r2 to the native_thread */
	current_r2 = current->native_thread_info;
	return current;
      }
      current = current->next;
    } while (current != head);
    /* r2 is non-null and not any tcr.  Assume that r2 is pthread
       struct pointer and that it's therefore safe to call get_tcr().
    */
    return get_tcr(true);
  }
#endif
}
  
  
void
suspend_resume_handler(int signo, siginfo_t *info, struct ucontext *context)
{
  TCR *tcr = get_interrupt_tcr(true);

  if (signo == thread_suspend_signal) {
    sigset_t wait_for;

    tcr->suspend_context = context;
    tcr->suspend_total++;
    sigfillset(&wait_for);
    SEM_RAISE(tcr->suspend);
    sigdelset(&wait_for, thread_resume_signal);
    do {
      sigsuspend(&wait_for);
    } while (tcr->suspend_context);
  
  } else {
    tcr->suspend_context = NULL;
  }
#ifdef DARWIN
  DarwinSigReturn(context);
#endif
}

void
thread_signal_setup()
{
  struct sigaction action;
  sigset_t mask, old_mask;
  
  sigemptyset(&mask);
  pthread_sigmask(SIG_SETMASK, &mask, &old_mask);

  thread_suspend_signal = SIG_SUSPEND_THREAD;
  thread_resume_signal = SIG_RESUME_THREAD;
  sigfillset(&action.sa_mask);
  sigdelset(&action.sa_mask,thread_suspend_signal);
  action.sa_flags = SA_RESTART | SA_SIGINFO;
  action.sa_sigaction = (void *) suspend_resume_handler;
  sigaction(thread_suspend_signal, &action, NULL);
  sigaction(thread_resume_signal, &action, NULL);
}
  

/*
  'base' should be set to the bottom (origin) of the stack, e.g., the
  end from which it grows.
*/
  
void
os_get_stack_bounds(LispObj q,void **base, unsigned *size)
{
  pthread_t p = (pthread_t)q;
#ifdef DARWIN
  *base = pthread_get_stackaddr_np(p);
  *size = pthread_get_stacksize_np(p);
#endif
#ifdef LINUX
  pthread_attr_t attr;
  
  pthread_getattr_np(p,&attr);
  pthread_attr_getstack(&attr, base, size);
  *(unsigned *)base += *size;
#endif
}

void *
new_semaphore(int count)
{
#ifdef LINUX
  sem_t *s = malloc(sizeof(sem_t));
  sem_init(s, 0, count);
  return s;
#endif
#ifdef DARWIN
  semaphore_t s;
  semaphore_create(mach_task_self(),&s, SYNC_POLICY_FIFO, count);
  return (void *)s;
#endif
}

RECURSIVE_LOCK
new_recursive_lock()
{
  extern int cache_block_size;
  void *p = calloc(1,sizeof(_recursive_lock)+cache_block_size-1);
  RECURSIVE_LOCK m = NULL;
  void *signal = new_semaphore(0);

  if (p) {
    m = (RECURSIVE_LOCK) ((((unsigned)p)+cache_block_size-1) & (~(cache_block_size-1)));
    m->malloced_ptr = p;
  }

  if (m && signal) {
    m->signal = signal;
    return m;
  }
  if (m) {
    free(p);
  }
  if (signal) {
    destroy_semaphore(&signal);
  }
  return NULL;
}

void
destroy_semaphore(void **s)
{
  if (*s) {
#ifdef LINUX
    sem_destroy((sem_t *)*s);
#endif
#ifdef DARWIN
    semaphore_destroy(mach_task_self(),((semaphore_t) *s));
#endif
    *s=NULL;
  }
}

void
tsd_set(LispObj key, void *datum)
{
  pthread_setspecific((pthread_key_t)key, datum);
}

void *
tsd_get(LispObj key)
{
  return pthread_getspecific((pthread_key_t)key);
}

void
dequeue_tcr(TCR *tcr)
{
  TCR *next, *prev;

  next = tcr->next;
  prev = tcr->prev;

  prev->next = next;
  next->prev = prev;
  tcr->prev = tcr->next = NULL;
}
  
void
enqueue_tcr(TCR *new)
{
  TCR *head, *tail;
  
  LOCK(lisp_global(TCR_LOCK),new);
  head = (TCR *)lisp_global(INITIAL_TCR);
  tail = head->prev;
  tail->next = new;
  head->prev = new;
  new->prev = tail;
  new->next = head;
  UNLOCK(lisp_global(TCR_LOCK),new);
}

  
TCR *
new_tcr(unsigned vstack_size, unsigned tstack_size)
{
  extern area* allocate_vstack(unsigned), *allocate_tstack(unsigned);
  area *a;
  TCR *tcr = calloc(1, sizeof(TCR));
  int i;

  lisp_global(TCR_COUNT) += (1<<fixnumshift);
  tcr->suspend = new_semaphore(0);
  tcr->resume = new_semaphore(0);
  tcr->reset_completion = new_semaphore(0);
  tcr->activate = new_semaphore(0);
  a = allocate_vstack(vstack_size);
  tcr->vs_area = a;
  tcr->save_vsp = (LispObj *) a->active;  
  a = allocate_tstack(tstack_size);
  tcr->ts_area = a;
  tcr->save_tsp = (LispObj *) a->active;
  tcr->valence = TCR_STATE_FOREIGN;
  tcr->interrupt_level = (-1<<fixnum_shift);
  tcr->lisp_fpscr.words.l = 0xd0;
  tcr->save_allocbase = tcr->save_allocptr = (void *) VOID_ALLOCPTR;
  tcr->tlb_limit = 8192;
  tcr->tlb_pointer = (LispObj *)malloc(tcr->tlb_limit);
  for (i = 0; i < (8192/sizeof(LispObj)); i++) {
    tcr->tlb_pointer[i] = (LispObj) no_thread_local_binding_marker;
  }
  tcr->shutdown_count = PTHREAD_DESTRUCTOR_ITERATIONS;
  return tcr;
}

void
shutdown_thread_tcr(void *arg)
{
  TCR *tcr = (void *)arg;

  area *vs, *ts, *cs;

  if (tcr->flags & (1<<TCR_FLAG_BIT_FOREIGN)) {
    TCR *current_tls = tsd_get(lisp_global(TCR_KEY));
    LispObj callback_macptr = nrs_FOREIGN_THREAD_CONTROL.vcell,
      callback_ptr = ((macptr *)(untag(callback_macptr)))->address;
    
    tsd_set(lisp_global(TCR_KEY), tcr);
    ((void (*)())callback_ptr)(1);
    tsd_set(lisp_global(TCR_KEY),current_tls);
  }
  
  if (--(tcr->shutdown_count) == 0) {
#ifdef DEBUG_THREAD_CLEANUP
    fprintf(stderr, "\nprocessing final shutdown request for pthread 0x%x tcr 0x%x", pthread_self(), tcr);
#endif
#ifdef DARWIN
    darwin_exception_cleanup(tcr);
#endif
  
    vs = tcr->vs_area;
    tcr->vs_area = NULL;
    ts = tcr->ts_area;
    tcr->ts_area = NULL;
    cs = tcr->cs_area;
    tcr->cs_area = NULL;
    if (vs) {
      condemn_area(vs);
    }
    if (ts) {
      condemn_area(ts);
    }
    if (cs) {
      condemn_area(cs);
    }
    destroy_semaphore(&tcr->suspend);
    destroy_semaphore(&tcr->resume);
    destroy_semaphore(&tcr->reset_completion);
    destroy_semaphore(&tcr->activate);
    tcr->osid = 0;
  } else {
    tsd_set(lisp_global(TCR_KEY), tcr);
#ifdef DEBUG_THREAD_CLEANUP
    fprintf(stderr, "\nprocessing early shutdown request for pthread 0x%x tcr 0x%x, tsd = 0x%x",
	    pthread_self(), tcr, tsd_get(lisp_global(TCR_KEY)));
#endif
  }
}

void *
current_native_thread_id()
{
  return ((void *)
#ifdef LINUX
          getpid()
#endif
#ifdef DARWIN
	  mach_thread_self()
#endif
	  );
}

void
thread_init_tcr(TCR *tcr, void *stack_base, unsigned stack_size)
{
  area *a, *register_cstack(BytePtr, unsigned);

  tcr->osid = current_thread_osid();
  tcr->native_thread_id = current_native_thread_id();
  a = register_cstack((BytePtr)stack_base, stack_size);
  tcr->cs_area = a;
  tcr->cs_limit = (LispObj)a->softlimit;
#ifdef LINUX
  tcr->native_thread_info = current_r2;
#endif
  tcr->errno_loc = &errno;
  tsd_set(lisp_global(TCR_KEY), tcr);
#ifdef DARWIN
  darwin_exception_init(tcr);
#endif
#ifdef LINUX
  linux_exception_init(tcr);
#endif
}

/*
  Register the specified tcr as "belonging to" the current thread.
  Under Darwin, setup Mach exception handling for the thread.
  Install cleanup handlers for thread termination.
*/
void
register_thread_tcr(TCR *tcr)
{
  void *stack_base;
  unsigned stack_size;

  os_get_stack_bounds(current_thread_osid(),&stack_base, &stack_size);
  thread_init_tcr(tcr, stack_base, stack_size);
  enqueue_tcr(tcr);
}


  
  
#ifndef MAP_GROWSDOWN
#define MAP_GROWSDOWN 0
#endif

Ptr
create_stack(int size)
{
  Ptr p = (Ptr) mmap(NULL,
		     (size_t)size,
		     PROT_READ | PROT_WRITE | PROT_EXEC,
		     MAP_PRIVATE | MAP_ANON | MAP_GROWSDOWN,
		     -1,	/* Darwin insists on this when not mmap()ing
				 a real fd */
		     0);
  if (p != (Ptr)(-1)) {
    *((size_t *)p) = size;
    return p;
  }
  allocation_failure(true, size);

}
  
void *
allocate_stack(unsigned size)
{
  return create_stack(size);
}

void
free_stack(void *s)
{
  size_t size = *((size_t *)s);
  munmap(s, size);
}

Boolean threads_initialized = false;

void
init_threads(void * stack_base, TCR *tcr)
{
  lisp_global(INITIAL_TCR) = (LispObj)tcr;
  pthread_key_create((pthread_key_t *)&(lisp_global(TCR_KEY)), shutdown_thread_tcr);
  thread_signal_setup();
  threads_initialized = true;
}


void *
lisp_thread_entry(void *param)
{
  thread_activation *activation = (thread_activation *)param;
  TCR *tcr = activation->tcr;
  sigset_t mask, old_mask;

  sigemptyset(&mask);
  pthread_sigmask(SIG_SETMASK, &mask, &old_mask);

  register_thread_tcr(tcr);
  tcr->vs_area->active -= 4;
  *(--tcr->save_vsp) = lisp_nil;
  enable_fp_exceptions();
  tcr->flags |= (1<<TCR_FLAG_BIT_AWAITING_PRESET);
  SEM_RAISE(activation->created);
  do {
    SEM_RAISE(tcr->reset_completion);
    SEM_WAIT(tcr->activate);
    /* Now go run some lisp code */
    start_lisp(tcr,0);
  } while (tcr->flags & (1<<TCR_FLAG_BIT_AWAITING_PRESET));
}

TCR *
xNewThread(unsigned control_stack_size,
	   unsigned value_stack_size,
	   unsigned temp_stack_size)

{
  thread_activation activation;
  activation.tcr = new_tcr(value_stack_size, temp_stack_size);
  activation.created = new_semaphore(0);
  create_system_thread(control_stack_size +(CSTACK_HARDPROT+CSTACK_SOFTPROT), 
		       NULL, 
		       lisp_thread_entry,
		       (void *) &activation);

  SEM_WAIT(activation.created);	/* Wait until thread's entered its initial function */
  destroy_semaphore(&activation.created);
  return activation.tcr;
}

Boolean
active_tcr_p(TCR *q)
{
  TCR *head = (TCR *)lisp_global(INITIAL_TCR), *p = head;
  
  do {
    if (p == q) {
      return true;
    }
    p = p->next;
  } while (p != head);
  return false;
}


OSErr
xDisposeThread(TCR *tcr)
{
  if (tcr != (TCR *)lisp_global(INITIAL_TCR)) {
    if (active_tcr_p(tcr) && (tcr != get_tcr(false))) {
      pthread_cancel((pthread_t)tcr->osid);
      return 0;
    }
  }
  return -50;
}

OSErr
xYieldToThread(TCR *target)
{
  Bug(NULL, "xYieldToThread ?");
  return 0;
}
  
OSErr
xThreadCurrentStackSpace(TCR *tcr, unsigned *resultP)
{
  Bug(NULL, "xThreadCurrentStackSpace ?");
  return 0;
}


LispObj
create_system_thread(size_t stack_size,
		     void* stackaddr,
		     void* (*start_routine)(void *),
		     void* param)
{
  pthread_attr_t attr;
  pthread_t returned_thread = (pthread_t) 0;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);  

  if (stack_size == MINIMAL_THREAD_STACK_SIZE) {
    stack_size = PTHREAD_STACK_MIN;
  }

  if (stackaddr != NULL) {
    /* Size must have been specified.  Sort of makes sense ... */
#ifdef DARWIN
    Fatal("no pthread_attr_setsetstack. "," Which end of stack does address refer to?");
#else
    pthread_attr_setstack(&attr, stackaddr, stack_size);
#endif
  } else if (stack_size != DEFAULT_THREAD_STACK_SIZE) {
    pthread_attr_setstacksize(&attr,stack_size);
  }

  /* 
     I think that's just about enough ... create the thread.
  */
  pthread_create(&returned_thread, &attr, start_routine, param);
  return (LispObj) returned_thread;
}

TCR *
get_tcr(Boolean create)
{
  TCR *current = (TCR *)tsd_get(lisp_global(TCR_KEY));

  if ((current == NULL) && create) {
    LispObj callback_macptr = nrs_FOREIGN_THREAD_CONTROL.vcell,
      callback_ptr = ((macptr *)(untag(callback_macptr)))->address;
    int i, nbindwords = 0;
    extern unsigned initial_stack_size;
    
    /* Make one. */
    current = new_tcr(initial_stack_size, MIN_TSTACK_SIZE);
    current->flags |= (1<<TCR_FLAG_BIT_FOREIGN);
    register_thread_tcr(current);
#ifdef DEBUG_TCR_CREATION
    fprintf(stderr, "\ncreating TCR for pthread 0x%x", pthread_self());
#endif
    current->vs_area->active -= 4;
    *(--current->save_vsp) = lisp_nil;
    nbindwords = ((int (*)())callback_ptr)(-1);
    for (i = 0; i < nbindwords; i++) {
      *(--current->save_vsp) = 0;
      current->vs_area->active -= 4;
    }
    ((void (*)())callback_ptr)(0);

  }
  
  return current;
}


Boolean
suspend_tcr(TCR *tcr)
{
  int suspend_count = atomic_incf(&(tcr->suspend_count));
  if (suspend_count == 1) {
    if (pthread_kill((pthread_t)tcr->osid, thread_suspend_signal) == 0) {
      SEM_WAIT(tcr->suspend);
    } else {
      /* A problem using pthread_kill.  On Darwin, this can happen
	 if the thread has had its signal mask surgically removed
	 by pthread_exit.  If the native (Mach) thread can be suspended,
	 do that and return true; otherwise, flag the tcr as belonging
	 to a dead thread by setting tcr->osid to 0.
      */
#ifdef DARWIN
      if (mach_suspend_tcr(tcr)) {
	tcr->flags |= TCR_FLAG_BIT_ALT_SUSPEND;
	return true;
      }
#endif
      tcr->osid = 0;
      return false;
    }
    return true;
  }
  return false;
}

Boolean
lisp_suspend_tcr(TCR *tcr)
{
  Boolean suspended;
  TCR *current = get_tcr(true);
  
  LOCK(lisp_global(TCR_LOCK),current);
  suspended = suspend_tcr(tcr);
  UNLOCK(lisp_global(TCR_LOCK),current);
  return suspended;
}
	 

Boolean
resume_tcr(TCR *tcr)
{
  int suspend_count = atomic_decf(&(tcr->suspend_count));
  if (suspend_count == 0) {
#ifdef DARWIN
    if (tcr->flags & TCR_FLAG_BIT_ALT_SUSPEND) {
      tcr->flags &= ~TCR_FLAG_BIT_ALT_SUSPEND;
      mach_resume_tcr(tcr);
      return true;
    }
#endif
    pthread_kill((pthread_t)tcr->osid, thread_resume_signal);
    return true;
  }
  return false;
}

Boolean
lisp_resume_tcr(TCR *tcr)
{
  Boolean resumed;
  TCR *current = get_tcr(true);
  
  LOCK(lisp_global(TCR_LOCK),current);
  resumed = resume_tcr(tcr);
  UNLOCK(lisp_global(TCR_LOCK), current);
  return resumed;
}

#ifdef DARWIN
lock_set_t mach_exception_lock_set;
#endif

void
suspend_other_threads()
{
  TCR *current = get_tcr(true), *other, *next;
  int dead_tcr_count = 0;

  LOCK(lisp_global(TCR_LOCK), current);
  for (other = current->next; other != current; other = other->next) {
    if ((other->osid != 0)) {
      suspend_tcr(other);
      if (other->osid == 0) {
	dead_tcr_count++;
      }
    } else {
      dead_tcr_count++;
    }
  }
  /* All other threads are suspended; can safely delete dead tcrs now */
  if (dead_tcr_count) {
    for (other = current->next; other != current; other = next) {
      next = other->next;
      if ((other->osid == 0))  {
	dequeue_tcr(other);
	free(other);
      }
    }
  }
}

void
resume_other_threads()
{
  TCR *current = get_tcr(true), *other;
  for (other = current->next; other != current; other = other->next) {
    resume_tcr(other);
  }
  UNLOCK(lisp_global(TCR_LOCK), current);
}

/*
  Try to take an rwquentry off of the rwlock's freelist; failing that,
  malloc one.  The caller owns the lock on the rwlock itself, of course.

*/
rwquentry *
recover_rwquentry(rwlock *rw)
{
  rwquentry *freelist = &(rw->freelist), 
    *p = freelist->next, 
    *follow = p->next;

  if (p == freelist) {
    p = NULL;
  } else {
    follow->prev = freelist;
    freelist->next = follow;
    p->prev = p->next = NULL;
    p->tcr = NULL;
    p->count = 0;
  }
  return p;
}

rwquentry *
new_rwquentry(rwlock *rw)
{
  rwquentry *p = recover_rwquentry(rw);

  if (p == NULL) {
    p = calloc(1, sizeof(rwquentry));
  }
  return p;
}


void
free_rwquentry(rwquentry *p, rwlock *rw)
{
  rwquentry 
    *prev = p->prev, 
    *next = p->next, 
    *freelist = &(rw->freelist),
    *follow = freelist->next;
  
  prev->next = next;
  next->prev = prev;
  p->prev = freelist;
  freelist->next = p;
  follow->prev = p;
  p->next = follow;
  p->prev = freelist;
}
  
void
add_rwquentry(rwquentry *p, rwlock *rw)
{
  rwquentry
    *head = &(rw->head),
    *follow = head->next;
  
  head->next = p;
  follow->prev = p;
  p->next = follow;
  p->prev = head;
}

rwquentry *
find_enqueued_tcr(TCR *target, rwlock *rw)
{
  rwquentry
    *head = &(rw->head),
    *p = head->next;

  do {
    if (p->tcr == target) {
      return p;
    }
    p = p->next;
  } while (p != head);
  return NULL;
}
    
rwlock *
rwlock_new()
{
  rwlock *rw = calloc(1, sizeof(rwlock));
  
  if (rw) {
    pthread_mutex_t *lock = calloc(1, sizeof(pthread_mutex_t));
    if (lock == NULL) {
      free (rw);
      rw = NULL;
    } else {
      pthread_cond_t *reader_signal = calloc(1, sizeof(pthread_cond_t));
      pthread_cond_t *writer_signal = calloc(1, sizeof(pthread_cond_t));
      if ((reader_signal == NULL) || (writer_signal == NULL)) {
        if (reader_signal) {
          free(reader_signal);
        } else {
          free(writer_signal);
        }
       
        free(lock);
        free(rw);
        rw = NULL;
      } else {
        pthread_mutex_init(lock, NULL);
        pthread_cond_init(reader_signal, NULL);
        pthread_cond_init(writer_signal, NULL);
        rw->lock = lock;
        rw->reader_signal = reader_signal;
        rw->writer_signal = writer_signal;
        rw->head.prev = rw->head.next = &(rw->head);
        rw->freelist.prev = rw->freelist.next = &(rw->freelist);
      }
    }
  }
  return rw;
}

/*
  no thread should be waiting on the lock, and the caller has just
  unlocked it.
*/
static void
rwlock_delete(rwlock *rw)
{
  pthread_mutex_t *lock = rw->lock;
  pthread_cond_t *cond;
  rwquentry *entry;

  rw->lock = NULL;
  cond = rw->reader_signal;
  rw->reader_signal = NULL;
  pthread_cond_destroy(cond);
  free(cond);
  cond = rw->writer_signal;
  rw->writer_signal = NULL;
  pthread_cond_destroy(cond);
  free(cond);
  while (entry = recover_rwquentry(rw)) {
    free(entry);
  }
  free(rw);
  pthread_mutex_unlock(lock);
  free(lock);
}

void
rwlock_rlock_cleanup(void *arg)
{
  pthread_mutex_unlock((pthread_mutex_t *)arg);
}
     
/*
  Try to get read access to a multiple-readers/single-writer lock.  If
  we already have read access, return success (indicating that the
  lock is held another time.  If we already have write access to the
  lock ... that won't work; return EDEADLK.  Wait until no other
  thread has or is waiting for write access, then indicate that we
  hold read access once.
*/
int
rwlock_rlock(rwlock *rw, TCR *tcr, struct timespec *waitfor)
{
  pthread_mutex_t *lock = rw->lock;
  rwquentry *entry;
  int err = 0;


  pthread_mutex_lock(lock);

  if (RWLOCK_WRITER(rw) == tcr) {
    pthread_mutex_unlock(lock);
    return EDEADLK;
  }

  if (rw->state > 0) {
    /* already some readers, we may be one of them */
    entry = find_enqueued_tcr(tcr, rw);
    if (entry) {
      entry->count++;
      rw->state++;
      pthread_mutex_unlock(lock);
      return 0;
    }
  }
  entry = new_rwquentry(rw);
  entry->tcr = tcr;
  entry->count = 1;

  pthread_cleanup_push(rwlock_rlock_cleanup,lock);

  /* Wait for current and pending writers */
  while ((err == 0) && ((rw->state < 0) || (rw->write_wait_count > 0))) {
    if (waitfor) {
      if (pthread_cond_timedwait(rw->reader_signal, lock, waitfor)) {
        err = errno;
      }
    } else {
      pthread_cond_wait(rw->reader_signal, lock);
    }
  }
  
  if (err == 0) {
    add_rwquentry(entry, rw);
    rw->state++;
  }

  pthread_cleanup_pop(1);
  return err;
}


/* 
   This is here to support cancelation.  Cancelation is evil. 
*/

void
rwlock_wlock_cleanup(void *arg)
{
  rwlock *rw = (rwlock *)arg;

  /* If this thread was the only queued writer and the lock
     is now available for reading, tell any threads that're
     waiting for read access.
     This thread owns the lock on the rwlock itself.
  */
  if ((--(rw->write_wait_count) == 0) &&
      (rw->state >= 0)) {
    pthread_cond_broadcast(rw->reader_signal);
  }
  
  pthread_mutex_unlock(rw->lock);
}

/*
  Try to obtain write access to the lock.
  If we already have read access, fail with EDEADLK.
  If we already have write access, increment the count that indicates
  that.
  Otherwise, wait until the lock is not held for reading or writing,
  then assert write access.
*/

int
rwlock_wlock(rwlock *rw, TCR *tcr, struct timespec *waitfor)
{
  pthread_mutex_t *lock = rw->lock;
  rwquentry *entry;
  int err = 0;


  pthread_mutex_lock(lock);
  if (RWLOCK_WRITER(rw) == tcr) {
    --RWLOCK_WRITE_COUNT(rw);
    --rw->state;
    pthread_mutex_unlock(lock);
    return 0;
  }
  
  if (rw->state > 0) {
    /* already some readers, we may be one of them */
    entry = find_enqueued_tcr(tcr, rw);
    if (entry) {
      pthread_mutex_unlock(lock);
      return EDEADLK;
    }
  }
  rw->write_wait_count++;
  pthread_cleanup_push(rwlock_wlock_cleanup,rw);

  while ((err == 0) && (rw->state) != 0) {
    if (waitfor) {
      if (pthread_cond_timedwait(rw->writer_signal, lock, waitfor)) {
        err = errno;
      }
    } else {
      pthread_cond_wait(rw->writer_signal, lock);
    }
  }
  if (err == 0) {
    RWLOCK_WRITER(rw) = tcr;
    RWLOCK_WRITE_COUNT(rw) = -1;
    rw->state = -1;
  }
  pthread_cleanup_pop(1);
  return err;
}

/*
  Sort of the same as above, only return EBUSY if we'd have to wait.
  In partucular, distinguish between the cases of "some other readers
  (EBUSY) another writer/queued writer(s)" (EWOULDBLOK) and "we hold a
  read lock" (EDEADLK.)
*/
int
rwlock_try_wlock(rwlock *rw, TCR *tcr)
{
  pthread_mutex_t *lock = rw->lock;
  rwquentry *entry;
  int ret = EBUSY;

  pthread_mutex_lock(lock);
  if ((RWLOCK_WRITER(rw) == tcr) ||
      ((rw->state == 0) && (rw->write_wait_count == 0))) {
    RWLOCK_WRITER(rw) = tcr;
    --RWLOCK_WRITE_COUNT(rw);
    --rw->state;
    pthread_mutex_unlock(lock);
    return 0;
  }
  
  if (rw->state > 0) {
    /* already some readers, we may be one of them */
    entry = find_enqueued_tcr(tcr, rw);
    if (entry) {
      ret = EDEADLK;
    }
  } else {
    /* another writer or queued writers */
    ret = EWOULDBLOCK;
  }
  pthread_mutex_unlock(rw->lock);
  return ret;
}

/*
  "Upgrade" a lock held once or more for reading to one held the same
  number of times for writing.
  Upgraders have higher priority than writers do
*/

int
rwlock_read_to_write(rwlock *rw, TCR *tcr)
{
}


int
rwlock_unlock(rwlock *rw, TCR *tcr)
{
  rwquentry *entry;

  pthread_mutex_lock(rw->lock);
  if (rw->state < 0) {
    /* Locked for writing.  By us ? */
    if (RWLOCK_WRITER(rw) != tcr) {
      pthread_mutex_unlock(rw->lock);
      /* Can't unlock: locked for writing by another thread. */
      return EPERM;
    }
    if (++RWLOCK_WRITE_COUNT(rw) == 0) {
      rw->state = 0;
      RWLOCK_WRITER(rw) = NULL;
      if (rw->write_wait_count) {
        pthread_cond_signal(rw->writer_signal);
      } else {
        pthread_cond_broadcast(rw->reader_signal);
      }
    }
    pthread_mutex_unlock(rw->lock);
    return 0;
  }
  entry = find_enqueued_tcr(tcr, rw);
  if (entry == NULL) {
    /* Not locked for reading by us, so why are we unlocking it ? */
    pthread_mutex_unlock(rw->lock);
    return EPERM;
  }
  if (--entry->count == 0) {
    free_rwquentry(entry, rw);
  }
  if (--rw->state == 0) {
    pthread_cond_signal(rw->writer_signal);
  }
  pthread_mutex_unlock(rw->lock);
  return 0;
}

        
int
rwlock_destroy(rwlock *rw)
{
  return 0;                     /* for now. */
}

/*
  A binding subprim has just done "twlle limit_regno,idx_regno" and
  the trap's been taken.  Extend the tcr's tlb so that the index will
  be in bounds and the new limit will be on a page boundary, filling
  in the new page(s) with 'no_thread_local_binding_marker'.  Update
  the tcr fields and the registers in the xp and return true if this
  all works, false otherwise.

  Note that the tlb was allocated via malloc, so realloc can do some
  of the hard work.
*/
Boolean
extend_tcr_tlb(TCR *tcr, 
               ExceptionInformation *xp, 
               unsigned limit_regno,
               unsigned idx_regno)
{
  unsigned
    index = (unsigned) (xpGPR(xp,idx_regno)),
    old_limit = tcr->tlb_limit,
    new_limit = align_to_power_of_2(index+1,12),
    new_bytes = new_limit-old_limit;
  LispObj 
    *old_tlb = tcr->tlb_pointer,
    *new_tlb = realloc(old_tlb, new_limit),
    *work;

  if (new_tlb == NULL) {
    return false;
  }
  
  work = (LispObj *) ((BytePtr)new_tlb+old_limit);

  while (new_bytes) {
    *work++ = no_thread_local_binding_marker;
    new_bytes -= sizeof(LispObj);
  }
  tcr->tlb_pointer = new_tlb;
  tcr->tlb_limit = new_limit;
  xpGPR(xp, limit_regno) = new_limit;
  return true;
}


