FROM perlur/centos-haskell-stack

RUN yum update -y && \
    yum install -y zlib-devel

WORKDIR /usr/src
COPY ./ /usr/src/kraken-exchange-api
WORKDIR kraken-exchange-api
RUN stack build
