directory lisp-kernel

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
handle SIG41 pass nostop noprint
handle SIG42 pass nostop noprint
handle SIGPWR pass nostop noprint

define ada 
 p *all_areas->succ
end

define _TCR
 p/x *(TCR *) $arg0
end

define tcr
 _TCR $r2
end

define regs
 p/x *(((struct pt_regs **)$arg0)[12])
end

define xpGPR
 p/x (((struct pt_regs **)$arg0)[12])->gpr[$arg1]
end

define xpPC
 p/x ((ExceptionInformation *)$arg0)->uc_mcontext.regs->nip
end

define hook-stop
handle SIGALRM nopass
end
     
define hook-run
handle SIGALRM pass
end
     
define hook-continue
handle SIGALRM pass
end

break Bug

display/i $pc
