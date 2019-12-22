FROM ubuntu:latest

RUN sed -i 's,^path-exclude=/usr/share/man/,#path-exclude=/usr/share/man/,' /etc/dpkg/dpkg.cfg.d/excludes

RUN apt-get update
RUN apt-get install -y apt-utils build-essential sudo git valgrind gdb
RUN apt-get install -y man manpages manpages-dev manpages-posix manpages-posix-dev
RUN mandb -c
RUN useradd -m docker && echo "docker:docker" | chpasswd && adduser docker sudo
USER docker
