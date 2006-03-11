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

break Bug

display/i $pc

handle SIGKILL pass nostop noprint
handle SIGILL pass nostop noprint
handle SIGSEGV pass nostop noprint
handle SIGBUS pass nostop noprint
handle SIGFPE pass nostop noprint
handle SIGUSR1 pass nostop noprint
handle SIGUSR2 pass nostop noprint
handle SIGEMT pass nostop noprint
