#  Rails On Lisp
#
#  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
#
#  Permission to use, copy, modify, and distribute this software for any
#  purpose with or without fee is hereby granted, provided that the above
#  copyright notice and this permission notice appear in all copies.
#
#  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
#  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
#  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
#  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
#  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
#  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
#  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

##  Defaults

ROL_DIR_ = lib/rol
ROL_SERVER_ = ${ROL_DIR}/server

SRCS_  != find * \( -name '.*' -prune \) \
          -or -name '[a-z]*.lisp' -print \
          -or -name '[a-z]*.asd' -print \

VIEWS_ != find app/views -type f -name '*[0-9a-z]' \

DATA_  != find data -type f -name '*.facts' \

LIBS_   = ${ROL_SERVER}/markdown.js

SBCL_ = env LC_ALL=en_US.UTF-8 sbcl

SBCL_OPTS_ = \
	--dynamic-space-size ${MEM} \
	--noinform \
	--end-runtime-options

SBCL_DEBUG_OPTS_ = \
	--disable-ldb \
	--lose-on-corruption \
	${SBCL_OPTS} \
	--eval '(declaim (optimize (debug 2) (safety 2) (speed 3) (space 1)))' \
	--disable-debugger

SBCL_BUILD_OPTS_ = \
	--disable-ldb \
	--lose-on-corruption \
	${SBCL_OPTS} \
	--eval '(declaim (optimize (debug 1) (safety 1) (speed 3) (space 1) (compilation-speed 0)))' \
	--disable-debugger

#  Allow local override

-include "config/local.mk"

#  Apply defaults

APP   ?= app
MEM   ?= 384
CORE  ?= ${APP}.sbcl
SRCS  ?= ${SRCS_}
VIEWS ?= ${VIEWS_}
DATA  ?= ${DATA_}
LIBS  ?= ${LIBS_}
ROL_DIR ?= ${ROL_DIR_}
ROL_SERVER ?= ${ROL_SERVER_}
SBCL ?= ${SBCL_}
SBCL_OPTS ?= ${SBCL_OPTS_}
SBCL_DEBUG_OPTS ?= ${SBCL_DEBUG_OPTS_}
SBCL_BUILD_OPTS ?= ${SBCL_BUILD_OPTS_}

APP_USER  ?= www
APP_GROUP ?= ${APP_USER}
APP_DIR   ?= /var/lib/service/${APP}/
WEB_USER  ?= www
WEB_GROUP ?= ${WEB_USER}
WEB_DIR   ?= /sites/${APP}/public/

##  Compile

FIND_PUBLIC = cd public && find . \
 \( -name \*~ -or -name *\#* -prune \) \
 -or -type f -print

LOAD_APP = \
	--load load

LOAD_TESTS = \
	--load ${ROL_SERVER}/load/tests \

LOAD_ASSETS = \
	--load ${ROL_SERVER}/load/assets \

build: assets ${CORE} run

core: ${CORE}

${CORE}: Makefile ${SRCS}
	${SBCL} ${SBCL_BUILD_OPTS} ${LOAD_APP} --eval '(build "${CORE}")'

run: Makefile ${ROL_SERVER}/run.in
	sed < ${ROL_SERVER}/run.in > run.tmp \
		-e 's/%APP%/${APP}/g' \
		-e 's/%CORE%/${CORE}/g' \
		-e 's/%APP_USER%/${APP_USER}/g' \
		-e 's/%APP_GROUP%/${APP_GROUP}/g' \
		-e 's/%WEB_USER%/${WEB_USER}/g' \
		-e 's/%WEB_GROUP%/${WEB_GROUP}/g'
	chmod 755 run.tmp
	mv run.tmp run
.PHONY: run

##  Assets

assets: clean-assets
	${SBCL} ${SBCL_BUILD_OPTS} ${LOAD_ASSETS} --quit < /dev/null

assets-:
	${SBCL} ${SBCL_BUILD_OPTS} ${LOAD_ASSETS} --quit

##  Clean

clean-assets:
	rm -rf public/assets

clean-build:
	rm -rf ${CORE} run run.tmp .mk.app-files
	find * -name '*.fasl' -print0 | xargs -0 rm -f

clean: clean-build clean-assets

distclean: clean
	find * \( -name \*~ -or -name *#* \) -print0 | xargs -0 rm -f

##  Debug

load:
	${SBCL} ${SBCL_DEBUG_OPTS} ${LOAD_APP} \
		--eval '(run)' \
		--quit

show:
	@echo APP = "${APP}"
	@echo APP_DIR = "${APP_DIR}"
	@echo APP_GROUP = "${APP_GROUP}"
	@echo APP_USER = "${APP_USER}"
	@echo SRCS = "${SRCS}"
	@echo WEB_DIR = "${WEB_DIR}"
	@echo WEB_GROUP = "${WEB_GROUP}"
	@echo WEB_USER = "${WEB_USER}"

URL ?= /
HOST ?= app.local

fetch:
	printf "GET ${URL} HTTP/1.0\nHost: ${HOST}\n\n" | nc ${HOST} 80

##  Test

test:
	${SBCL} ${SBCL_DEBUG_OPTS} ${LOAD_APP} ${LOAD_TESTS} \
		--eval '(run-tests)'

##  Install

NODE_MODULES != find ${ROL_DIR}/node_modules

install: install-app install-web

install-app:
	echo "${CORE} ${VIEWS} ${DATA} run ${LIBS} ${NODE_MODULES}" | xargs -rn 1 | ${SUDO} cpio -pdmu ${APP_DIR}
	${SUDO} mkdir -p ${APP_DIR}/log
	cd ${APP_DIR} && echo "${CORE} ${VIEWS} ${DATA} ${LIBS} ${NODE_MODULES}" | ${SUDO} xargs -r chmod -R u=rwX,g=rX,o=
	cd ${APP_DIR} && echo "${CORE} ${VIEWS} ${LIBS} ${NODE_MODULES}" | ${SUDO} xargs -r chown -R "root:${APP_GROUP}"
	cd ${APP_DIR} && echo "${DATA}" | ${SUDO} xargs -r chown -R "${APP_USER}:${APP_GROUP}"

install-web:
	${FIND_PUBLIC} | ${SUDO} rsync -lstv --files-from=/dev/stdin . ${WEB_DIR}
	${SUDO} chmod -R u=rwX,g=rX,o= ${WEB_DIR}
	${SUDO} chown -R "${WEB_USER}:${WEB_GROUP}" ${WEB_DIR}

.PHONY: build assets clean-assets clean-build clean distclean install load show
