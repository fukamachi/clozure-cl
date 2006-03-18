define x86_lisp_string
x/s $arg0-5
end

define x86pname
set $temp=*((long *)((long)($arg0-6)))
x86_lisp_string $temp
end


define pname
 x86pname $arg0
end

define l
 call print_lisp_object($arg0)
end

define lw
 l $r13
end

define clobber_breakpoint
  set *(short *)($pc-2)=0x9090
end

break Bug

display/i $pc

handle SIGKILL pass nostop noprint
handle SIGILL pass nostop noprint
handle SIGSEGV pass nostop noprint
handle SIGBUS pass nostop noprint
handle SIGFPE pass nostop noprint
handle SIGEMT pass nostop noprint
