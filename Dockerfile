FROM ubuntu:20.04

LABEL maintainer="ashalper@usgs.gov"

RUN apt-get -y update
RUN apt-get -y dist-upgrade
RUN apt-get -y install libcurl4-openssl-dev

# install DOI SSL intercept root certificate if reachable
RUN if wget http://sslhelp.doi.net/docs/DOIRootCA2.cer ; then \
        mkdir -p /usr/local/share/ca-certificates ; \
        mv DOIRootCA2.cer /usr/local/share/ca-certificates/DOIRootCA2.crt ; \
        update-ca-certificates ; \
    fi
