ENV = env

VIRTUALENV_SYSTEM_SITE_PACKAGES ?= true
VIRTUALENV = \
	VIRTUALENV_SYSTEM_SITE_PACKAGES=$(VIRTUALENV_SYSTEM_SITE_PACKAGES) \
		virtualenv --python=$(PYTHON)
USE_JEDI_DEV ?=
PYTHON ?= python
CARTON ?= carton
EMACS ?= emacs


EL4T_SCRIPT = tools/el4t/emacs.sh
EL4T_CARTON = EL4T_EMACS=${EMACS} EMACS=${EL4T_SCRIPT} ${CARTON}
EL4T_CARTON_EMACS = ${EL4T_CARTON} exec ${EL4T_SCRIPT}

.PHONY : test test-1 tryout clean-elpa requirements env clean-env clean \
	print-deps travis-ci doc

test: elpa requirements
	$(ENV)/bin/pip install --use-mirrors pytest
	${MAKE} test-1

test-1:
	rm -f elpa/mocker-*/*elc  # workaround a bug in mocker.el
	${EL4T_CARTON_EMACS} -Q -batch \
		-L . -l test-jedi.el -f ert-run-tests-batch-and-exit
	$(ENV)/bin/py.test test_jediepcserver.py

compile: elpa clean-elc
	${EL4T_CARTON_EMACS} -Q -batch \
		-L . -f batch-byte-compile *.el

clean-elc:
	rm -rf *.elc

tryout: compile requirements
	${EL4T_CARTON_EMACS} -Q -L . -l tryout-jedi.el

doc: elpa
	make -C doc html

${EL4T_SCRIPT}:
	git submodule update --init

elpa: ${EL4T_SCRIPT}
	mkdir -p elpa
	${EL4T_CARTON} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

requirements: env
	$(ENV)/bin/pip install --requirement requirements.txt
	if [ -n "${USE_JEDI_DEV}" ]; then ${MAKE} install-jedi-dev; fi

install-jedi-dev:
	$(ENV)/bin/pip install -U https://github.com/davidhalter/jedi/archive/dev.zip

env: $(ENV)/bin/activate
$(ENV)/bin/activate:
	$(VIRTUALENV) $(ENV)

clean-env:
	rm -rf $(ENV)

clean-el: clean-elpa clean-elc
clean: clean-env clean-el

print-deps: elpa requirements
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	${EL4T_CARTON_EMACS} -Q -batch -l jedi.el -f jedi:print-jedi-version
	ls -d $(ENV)/lib/python*/site-packages/*egg-info
	@echo "------------------------------------------------------------"

travis-ci: print-deps test



# Run test against Emacs listed in ${EL4T_EMACS_LIST}.
# This is for running tests for multiple Emacs versions.
# This is not used in Travis CI.  Usage::
#
#     make EL4T_EMACS_LIST="emacs emacs-snapshot emacs23" test-all
#
# See: http://stackoverflow.com/a/12110773/727827
#
# Use ${EL4T_MET_MAKEFLAGS} to do the tests in parallel.
#
#    EL4T_MET_MAKEFLAGS=-j4

JOBS := $(addprefix job-,${EL4T_EMACS_LIST})
.PHONY: ${JOBS}

${JOBS}: job-%:
	${MAKE} EMACS=$* clean-el elpa
	${MAKE} EMACS=$* ${EL4T_MET_MAKEFLAGS} test-1

test-all: requirements ${JOBS}
