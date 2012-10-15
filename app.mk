##  Compile

APP ?= app

CORE = ${APP}.sbcl-core

SRCS !=	find * \( -name '.*' -prune \) -or -name '[a-z]*.lisp' -print

VIEWS =	app/views/*/*.html \
	app/views/*/*.js

DATA =	data/*.facts

FIND_PUBLIC = find public \
	\( -name \*~ -or -name *\#* -prune \) -or \
	-type f -print

LOWH_TRIANGLE_SERVER = lib/lowh-triangle-server

SBCL = env LC_ALL=en_US.UTF-8 sbcl

SBCL_OPTS = \
	--dynamic-space-size 256 \
	--noinform \
	--end-runtime-options

SBCL_DEBUG_OPTS = \
	${SBCL_OPTS} \
	--eval '(declaim (optimize (debug 3) (safety 2) (speed 0) (space 0)))'

SBCL_BUILD_OPTS = \
	--disable-ldb \
	--lose-on-corruption \
	${SBCL_OPTS} \
	--eval '(declaim (optimize (debug 0) (safety 2) (speed 3) (space 1)))' \
	--disable-debugger

LOAD_LOAD = --load ${LOWH_TRIANGLE_SERVER}/load

build: ${CORE} run

${CORE}: Makefile ${SRCS}
	${MAKE} clean
	${SBCL} ${SBCL_BUILD_OPTS} ${LOAD_LOAD} --eval '(build "${CORE}")'

run: Makefile ${LOWH_TRIANGLE_SERVER}/run.in
	sed < ${LOWH_TRIANGLE_SERVER}/run.in > run.tmp \
		-e 's/%APP%/${APP}/g' \
		-e 's/%CORE%/${CORE}/g'

	chmod 755 run.tmp
	mv run.tmp run

##  Clean

clean:
	rm -f ${CORE} run run.tmp
	find * -name '*.fasl' -print0 | xargs -0 rm -f

distclean:
	find * \( -name \*~ -or -name *#* \) -print0 | xargs -0 rm -f

##  Debug

load:
	${SBCL} ${SBCL_DEBUG_OPTS} ${LOAD_LOAD} --eval '(run)'

show:
	@echo APP = "${APP}"
	@echo SRCS = "${SRCS}"
	@echo PUBLIC_FILES = "${PUBLIC_FILES}"

URL ?= /
fetch:
	printf "GET ${URL} HTTP/1.0\nHost: www-nv.local\n\n" | nc www-nv.local 80

##  Install

APP_USER ?= www:www
APP_DIR  ?= /var/lib/service/${APP}
WEB_USER ?= ${APP_USER}
WEB_DIR  ?= /sites/${APP}

install: install-app install-web

install-app: build
	ls -1 ${CORE} run ${VIEWS} ${DATA} | ${SUDO} cpio -pdmu ${APP_DIR}
	${SUDO} mkdir -p ${APP_DIR}/log
	${SUDO} chmod -R u=rwX,g=rX,o= ${APP_DIR}
	${SUDO} chown -R ${APP_USER} ${APP_DIR}

install-web:
	${FIND_PUBLIC} | ${SUDO} cpio -pdmu ${WEB_DIR}
	${SUDO} chmod -R u=rwX,g=rX,o= ${WEB_DIR}
	${SUDO} chown -R ${WEB_USER} ${WEB_DIR}

.PHONY: clean distclean install load show

.if exists("config/local.mk")
.  include "config/local.mk"
.endif
