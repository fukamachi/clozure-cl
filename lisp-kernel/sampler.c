/*
   Copyright (C) 2002 Clozure Associates
   This file is part of Opensourced MCL.

   Opensourced MCL is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   Opensourced MCL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#include <pthread.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#ifdef LINUX
/* Linux has POSIX semaphores */
#include <semaphore.h>
#endif
#ifdef DARWIN
/* We have to use Mach semaphores, even if we're otherwise 
   using POSIX signals, etc. */
#include <mach/task.h>
#include <mach/semaphore.h>
#endif
#ifndef USE_MACH
#include <sys/ucontext.h>
#endif

#ifdef DARWIN
#include "sigcontext.h"
#endif

#ifdef SIGRTMIN
#define SIG_SUSPEND_THREAD (SIGRTMIN+6)
#define SIG_RESUME_THREAD (SIG_SUSPEND_THREAD+1)
#else
#define SIG_SUSPEND_THREAD SIGUSR1
#define SIG_RESUME_THREAD SIGUSR2
#endif

#ifdef LINUX
typedef sem_t SEMAPHORE;
#define SEM_INIT(s, count) sem_init(s, 0, count);
#define SEM_WAIT(s) sem_wait(s)
#define SEM_POST(s) sem_post(s);
#endif

#ifdef DARWIN
typedef semaphore_t SEMAPHORE;
#define SEM_INIT(s, count) semaphore_create(mach_task_self(),s,SYNC_POLICY_FIFO,count)
#define SEM_WAIT(s) semaphore_wait(*s)
#define SEM_POST(s) semaphore_signal(*s)
#endif

pthread_key_t shared_key = 0;

typedef
#ifdef DARWIN
struct linux_sigcontext_struct
#endif
#ifdef LINUX
struct sigcontext
#endif
signal_context;

typedef struct thread_info {
  struct thread_info* prev;
  struct thread_info* *next;
  pthread_t id;
  signal_context *state;
  SEMAPHORE suspended;
  SEMAPHORE resumed;
} thread_info;




#ifdef DARWIN
void
mach_thread_state_to_pt_regs(ppc_thread_state_t *ts, struct pt_regs *pt)
{
  int i;
  unsigned *gprsrc=&(ts->r0), *gprdest = (unsigned *)pt;
  for (i = 0; i < 31; i++) {
    *gprdest++ = *gprsrc++;
  }
  pt->nip = ts->srr0;
  pt->msr = ts->srr1;
  pt->ctr = ts->ctr;
  pt->link = ts->lr;
  pt->ccr = ts->cr;
  pt->xer = ts->xer;
}
#endif
	    
#ifndef USE_MACH
void
handle_thread_suspend(int signo, 
#ifdef DARWIN
		      siginfo_t *siginfo,
#endif
		      void *context)
{
#ifdef DARWIN
  struct linux_sigcontext_struct sc;
  struct pt_regs pt;
  struct ucontext *uc = (struct ucontext *)context;
#endif
  thread_info *info;
  if (signo == SIG_SUSPEND_THREAD) {
    /* Sleep until we get a SIG_RESUME_THREAD signal */
    info = pthread_getspecific(shared_key);
#ifdef DARWIN
    sc.regs = &pt;
    mach_thread_state_to_pt_regs(&(uc->uc_mcontext->ss), &pt);
    info->state = &sc;
#else
    info->state = (signal_context *)context;
#endif
    SEM_POST(&(info->suspended));
    SEM_WAIT(&(info->resumed));
  }
}
#endif

  

void
suspend_thread(thread_info *info)
{
  pthread_t tid = info->id;
#ifdef USE_MACH
  mach_port_t mach_thread = pthread_mach_thread_np(tid);
  thread_suspend(mach_thread);
#if 0
   thread_abort(mach_thread);	/* Ensure not in a syscall */
#endif
  if (info->state) {
    ppc_thread_state_t ts;
    mach_msg_type_number_t thread_state_count;

    thread_state_count = MACHINE_THREAD_STATE_COUNT;
    thread_get_state(mach_thread, 
		     MACHINE_THREAD_STATE,	/* GPRs, some SPRs  */
		     (thread_state_t)&ts,
		     &thread_state_count);

    mach_thread_state_to_pt_regs(&ts, ((signal_context *)info->state)->regs);
  }
#else
  pthread_kill(tid, SIG_SUSPEND_THREAD);
  SEM_WAIT(&(info->suspended));
#endif
}

void
resume_thread(thread_info *info)
{
  pthread_t tid = info->id;
#ifdef USE_MACH
  mach_port_t mach_thread = pthread_mach_thread_np(tid);
  thread_resume(mach_thread);
#else
  SEM_POST(&(info->resumed));
#endif
}

#ifndef USE_MACH
typedef
void (*signal_handler)(int,
		       siginfo_t *,
		       void *);
#endif


void
show_thread_context(FILE *out, signal_context *context)
{
  if (context) {
    int a, b, c, d;
    struct pt_regs *regs = context->regs;
    char buf[128];
    struct timeval now;
  
    gettimeofday(&now, NULL);

    strftime(buf,sizeof(buf)-1, "%D %T", localtime((clock_t *)&now.tv_sec));
    fprintf(out,"%s.%06d:\n\n", buf, now.tv_usec);
    for (a = 0, b = 8, c = 16, d = 24; a < 8; a++, b++, c++, d++) {
      fprintf(out,"r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X  r%02d = 0x%08X\n",
	      a, regs->gpr[a],
	      b, regs->gpr[a],
	      c, regs->gpr[a],
	      d, regs->gpr[a]);
    }
    fprintf(out, "\n PC = 0x%08X   LR = 0x%08X  CTR = 0x%08X  CCR = 0x%08X\n",
	    regs->nip, regs->link, regs->ctr, regs->ccr);
    fprintf(out, "XER = 0x%08X  MSR = 0x%08X  DAR = 0x%08X  DSISR = 0x%08X\n",
	    regs->xer, regs->msr, regs->dar, regs->dsisr);
    fprintf(out,"\n\n");
  }
}

void *
sampler_loop(void *primary_pthread_info)
{
  thread_info *info = (thread_info *)primary_pthread_info;
  FILE *log = fopen("sampler.log", "w");
#ifdef USE_MACH
  signal_context sc;
  struct pt_regs pt;
  sc.regs = &pt;
  info->state = &sc;
#endif
  while (1) {
    usleep(random() % 100000);
    suspend_thread(info);
    show_thread_context(log, info->state);
    fflush(log);
    resume_thread(info);
  }
}

pthread_t
create_sampler_thread(thread_info *info)
{
  pthread_t the_thread;
  pthread_attr_t attr;
  int err, error_code;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

  if (err = pthread_create(&the_thread, NULL, sampler_loop, info)) {
    error_code = errno;
    fprintf(stderr, "Can't create thread. Err = %d, errno = %d\n", err, error_code);
    exit(1);
  }
  return the_thread;
}
void
start_sampler_thread()
{
  pthread_t sampler_thread;
  thread_info *info = malloc(sizeof(thread_info));
#ifndef USE_MACH
  struct sigaction action;
  sigfillset(&action.sa_mask);
#ifdef LINUX
  action.sa_flags = SA_RESTART;
#endif
#ifdef DARWIN
  action.sa_flags = SA_SIGINFO | SA_RESTART;
#endif
  action.sa_sigaction = (signal_handler)handle_thread_suspend;

  sigaction(SIG_SUSPEND_THREAD, &action, NULL);
#endif
  pthread_key_create(&shared_key, NULL);
  SEM_INIT(&(info->suspended), 0);
  SEM_INIT(&(info->resumed), 0);
  info->state = NULL;
  info->id = pthread_self();
  pthread_setspecific(shared_key,info);
  sampler_thread = create_sampler_thread(info);
}
