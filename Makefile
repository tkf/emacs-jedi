ENV = env

PYTHON ?= python
CARTON ?= carton
EMACS ?= emacs

.PHONY : test test-1 tryout clean-elpa requirements env clean-env clean \
	print-deps travis-ci

test: elpa requirements
	make EMACS=${EMACS} CARTON=${CARTON} test-1

test-1:
	EMACS=${EMACS} ${CARTON} exec ${EMACS} -Q -batch \
		-L . -l test-jedi.el -f ert-run-tests-batch-and-exit
	nosetests test_jediepcserver.py

compile: elpa
	rm -rf *.elc
	EMACS=$(EMACS) ${CARTON} exec ${EMACS} -Q -batch \
		-L . -f batch-byte-compile *.el

tryout: compile requirements
	EMACS=$(EMACS) ${CARTON} exec ${EMACS} -Q -L . -l tryout-jedi.el

elpa:
	${CARTON} install

clean-elpa:
	rm -rf elpa

requirements: env
	$(ENV)/bin/pip install --requirement requirements.txt

env: $(ENV)/bin/activate
$(ENV)/bin/activate:
	virtualenv --python=$(PYTHON) $(ENV)

clean-env:
	rm -rf $(ENV)

clean: clean-env clean-elpa

print-deps: requirements
	@echo "------------------- Python dependencies --------------------"
	$(ENV)/bin/python --version
	$(ENV)/bin/python print_deps.py
	ls -d $(ENV)/lib/python*/site-packages/*egg-info
	@echo "------------------------------------------------------------"

travis-ci: print-deps test
