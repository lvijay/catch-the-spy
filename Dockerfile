FROM debian:stretch
LABEL maintainer="laksvij@hawk.iit.edu"

ENV SBCL_VERSION=1.5.1

WORKDIR /usr/local/src/

# hadolint ignore=DL3003,DL3008
RUN set -x \
    && apt-get update \
    && apt-get install -y --no-install-recommends wget gnupg ca-certificates make dirmngr bzip2 \
    && wget https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 \
    && wget https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-crhodes.asc \
    && GNUPGHOME="$(mktemp -d)" \
    && export GNUPGHOME \
    && export CRHODES_KEY=D6839CA0A67F74D9DFB70922EBD595A9100D63CD \
    && (gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys ${CRHODES_KEY} \
       || gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys ${CRHODES_KEY} \
       || gpg --batch --keyserver keyserver.ubuntu.com --recv-keys ${CRHODES_KEY} \
       || gpg --batch --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys ${CRHODES_KEY} \
       || gpg --batch --keyserver pgp.mit.edu --recv-keys ${CRHODES_KEY}) \
    && gpg --batch --verify sbcl-${SBCL_VERSION}-crhodes.asc \
    && bunzip2 sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 \
    && gpg --batch --decrypt sbcl-${SBCL_VERSION}-crhodes.asc > sbcl-${SBCL_VERSION}-crhodes.txt \
    && grep sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar sbcl-${SBCL_VERSION}-crhodes.txt > sum-file.txt \
    && sha256sum -c sum-file.txt \
    && rm -rf "$GNUPGHOME" sbcl-${SBCL_VERSION}-crhodes.asc sbcl-${SBCL_VERSION}-crhodes.txt sum-file.txt \
    && tar xf sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar \
    && (cd sbcl-${SBCL_VERSION}-x86-64-linux/ && sh install.sh) \
    && rm -rf sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar sbcl-${SBCL_VERSION}-x86-64-linux/ \

RUN cd /tmp \
    && wget https://beta.quicklisp.org/quicklisp.lisp \
    && echo "(load \"quicklisp.lisp\") (quicklisp-quickstart:install :path \"/opt/quicklisp\") (ql::without-prompting (ql:add-to-init-file))" | sbcl \
    && cp $HOME/.sbclrc /etc/sbclrc 

RUN sbcl --eval '(ql:quickload :hunchentoot)'
RUN sbcl --eval '(ql:quickload :cl-who)'

WORKDIR /root

ADD spy.lisp spy.lisp

ENV LANG=C.UTF-8

ENTRYPOINT exec sbcl --load spy.lisp

EXPOSE 4242
