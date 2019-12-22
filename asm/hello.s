#
# hello.s
# Hello World for x86 assembly - Mac OS X
#
# as -o hello.o hello.s
# ld -e start -o hello hello.o
#

.text
.globl start

hello:
  .ascii "hello, world!\n"
  len = . - hello

syscall:
  # pop the return address off the stack and into EDX
  # for the sysenter call but then push it back so that
  # the system call itself has
  popl %edx
  pushl %edx
  movl %esp, %ecx
  sysenter

start:
#
# len = write(1, hello, sizeof(hello))
#
  pushl $len            # pass the length of the string
  pushl $hello          # pass the pointer to the string
  pushl $1              # pass stdout
  movl  $0x04, %eax     # SYS_write syscall = 4
  call syscall
  add   $16, %esp       # clear the stack (3 args + return address)

#
# exit(length)
#
  pushl %eax            # push return value from previous syscall to arg stack
  movl  $1, %eax        # SYS_exit syscall = 1
  call syscall
