define lisp_string
x/s ($arg0-2)
end

define pname
lisp_string (*($arg0-2))
end

# show entrypoint of function
define entry
if ((($arg0&7)!=6)||((*(char*)$arg0-3)!=0x2a)) then
printf "Not a function - 0x~08x", $arg0
else
x/i 
end
end

handle SIGILL pass nostop noprint
handle SIGSEGV pass nostop noprint
handle SIGBUS pass nostop noprint
handle SIGFPE pass nostop noprint
handle SIGUSR1 pass nostop noprint
handle SIGUSR2 pass nostop noprint
handle SIGEMT pass nostop noprint


define ada 
 p *all_areas->succ
end

define _TCR
 p/x *(TCR *) $arg0
end

define tcr
 _TCR ($r2-0x7000)
end

define regs
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext.ss
end

define xpGPR
 p/x ((unsigned long *)&((((ExceptionInformation *)$arg0)->uc_mcontext.ss)))[2+$arg1]
end

define xpPC
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext.ss.srr0
end


break Bug

display/i $pc
