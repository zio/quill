FROM ubuntu:14.04
MAINTAINER gustavo.amigo@gmail.com

RUN apt-get update; \
  apt-get install -y python curl postgresql-client mysql-client > /dev/null

RUN  cd /opt ; \
     curl http://mirror.nbtelecom.com.br/apache/cassandra/2.1.12/apache-cassandra-2.1.12-bin.tar.gz | tar zx

ENV PATH /opt/apache-cassandra-2.1.12/bin:$PATH

