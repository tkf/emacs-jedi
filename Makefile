ENV = env

VIRTUALENV_SYSTEM_SITE_PACKAGES ?= true
VIRTUALENV = \
	VIRTUALENV_SYSTEM_SITE_PACKAGES=$(VIRTUALENV_SYSTEM_SITE_PACKAGES) \
		virtualenv --python=$(PYTHON)
PIP_INSTALL = $(ENV)/$(BINDIR)/pip install --use-mirrors
JEDI_DEV_URL = https://github.com/davidhalter/jedi/archive/dev.zip

PYTHON ?= python
CARTON ?= carton
export EMACS ?= emacs

BINDIR ?= bin

VIRTUAL_EMACS = ${CARTON} exec ${EMACS}

.PHONY : test test-1 tryout clean-elpa requirements env clean-env clean \
	print-deps travis-ci doc

test: elpa requirements
	${MAKE} test-1

test-1:
	rm -f elpa/mocker-*/*elc  # workaround a bug in mocker.el
	${VIRTUAL_EMACS} -Q -batch \
		-L . -l test-jedi.el -f ert-run-tests-batch-and-exit
	tox

compile: elpa clean-elc
	${VIRTUAL_EMACS} -Q -batch \
		-L . -f batch-byte-compile *.el

clean-elc:
	rm -rf *.elc

tryout: compile requirements
	${VIRTUAL_EMACS} -Q -L . -l tryout-jedi.el

doc: elpa
	make -C doc html

ensure-git:
	test -d .git  # Running task that can be run only in git repository

elpa: Carton
	mkdir -p elpa
	${CARTON} install 2> elpa/install.log
	touch $@

clean-elpa:
	rm -rf elpa

requirements: env
	${PIP_INSTALL} --requirement requirements.txt

install-jedi-dev:
	${PIP_INSTALL} --upgrade ${JEDI_DEV_URL}

env: $(ENV)/$(BINDIR)/activate
$(ENV)/$(BINDIR)/activate:
	$(VIRTUALENV) $(ENV)

clean-env:
	rm -rf $(ENV)

clean-el: clean-elpa clean-elc
clean: clean-env clean-el

print-deps: elpa requirements
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	${VIRTUAL_EMACS} -Q -batch -l jedi.el -f jedi:print-jedi-version
	ls -d $(ENV)/*/python*/site-packages/*egg-info
	@echo "------------------------------------------------------------"

before-test: requirements
	tox --notest

travis-ci: print-deps test



# Run test against Emacs listed in ${EMACS_LIST}.
# This is for running tests for multiple Emacs versions.
# This is not used in Travis CI.  Usage::
#
#     make EMACS_LIST="emacs emacs-snapshot emacs23" test-all
#
# See: http://stackoverflow.com/a/12110773/727827
#
# Use ${MET_MAKEFLAGS} to do the tests in parallel.
#
#    MET_MAKEFLAGS=-j4

JOBS := $(addprefix job-,${EMACS_LIST})
.PHONY: ${JOBS}

${JOBS}: job-%:
	${MAKE} EMACS=$* clean-el elpa
	${MAKE} EMACS=$* ${MET_MAKEFLAGS} test-1

test-all: requirements ${JOBS}



### Packaging
#
# Create dist/${PACKAGE}-${VERSION}.tar.gz ready for distribution.
#
# See: (info "(elisp) Multi-file Packages")
PACKAGE = jedi
VERSION = $(shell grep ';; Version:' jedi.el | sed 's/^.* \([0-9].*\)$$/\1/')
DIST_FILES = jedi-pkg.el jedi.el jediepcserver.py \
	requirements.txt Makefile tryout-jedi.el

.PHONY: dist ${PACKAGE}-${VERSION}.tar.gz ${PACKAGE}-${VERSION} \
	clean-dist clean-dist-all

dist: clean-dist
	${MAKE} dist-1

dist-1: dist/${PACKAGE}-${VERSION}.tar dist/${PACKAGE}-${VERSION}.tar.gz

dist/${PACKAGE}-${VERSION}.tar: ${PACKAGE}-${VERSION}.tar
${PACKAGE}-${VERSION}.tar: ${PACKAGE}-${VERSION}
	tar --directory dist -cvf dist/$@ $<

dist/${PACKAGE}-${VERSION}.tar.gz: ${PACKAGE}-${VERSION}.tar.gz
${PACKAGE}-${VERSION}.tar.gz: ${PACKAGE}-${VERSION}
	tar --directory dist -cvzf dist/$@ $<

${PACKAGE}-${VERSION}: dist/${PACKAGE}-${VERSION}
dist/${PACKAGE}-${VERSION}:
	mkdir -p $@
	cp -v ${DIST_FILES} $@

clean-dist:
	rm -rf dist/${PACKAGE}-${VERSION}*

clean-dist-all:
	rm -rf dist



### Package installation
PACKAGE_USER_DIR =
TEST_PACKAGE_DIR = dist/test

install-dist:
	test -d '${PACKAGE_USER_DIR}'
	${EMACS} --batch -Q \
	-l package \
        --eval " \
        (add-to-list 'package-archives \
             '(\"marmalade\" . \"http://marmalade-repo.org/packages/\") t)" \
	--eval '(setq package-user-dir "${PWD}/${PACKAGE_USER_DIR}")' \
	--eval '(package-list-packages)' \
	--eval '(package-install-file "${PWD}/dist/${PACKAGE}-${VERSION}.tar")'

test-install: dist/${PACKAGE}-${VERSION}.tar
	rm -rf ${TEST_PACKAGE_DIR}
	mkdir -p ${TEST_PACKAGE_DIR}
	${MAKE} install-dist PACKAGE_USER_DIR=${TEST_PACKAGE_DIR}

test-install-requirement: test-install
	${MAKE} --directory ${TEST_PACKAGE_DIR}/${PACKAGE}-${VERSION} \
		requirements



### GH pages
MAKE_GH_PAGES = $(MAKE) --directory doc --file gh-pages.mk

gh-pages-latest:
	$(MAKE_GH_PAGES)

# Publish released documentation.  This task can be run only when the
# current revision has tag (i.e., released).
gh-pages-released:
	# Make sure it's on tag
	git describe --tags --dirty | grep -v -
	# Run doc/gh-pages.mk
	$(MAKE_GH_PAGES) DOC_VER=released
