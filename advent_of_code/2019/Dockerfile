FROM ubuntu:latest
RUN apt-get update
RUN apt-get install -y --no-install-recommends apt-utils build-essential sudo git valgrind
RUN useradd -m docker && echo "docker:docker" | chpasswd && adduser docker sudo
USER docker
