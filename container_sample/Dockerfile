FROM ubuntu:15.10
MAINTAINER K Chugalinskiy <kchugalinskiy@yandex.ru>
RUN apt-get update && apt-get install -y erlang yaws
ADD ttask.deb /tmp/ttask.deb
COPY yaws.conf /etc/yaws/yaws.conf
RUN dpkg -i /tmp/ttask.deb
EXPOSE 8080
ENTRYPOINT ["yaws"]