FROM ubuntu:bionic

RUN apt update 
RUN apt install curl gcc python3 -y

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y


COPY docker_entrypoint.sh /entrypoint.sh


ENTRYPOINT [ "/entrypoint.sh" ]
