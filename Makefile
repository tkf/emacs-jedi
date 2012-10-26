ENV = env
ifndef PYTHON
	PYTHON = python
endif

ifndef CARTON
	CARTON = carton
endif

ifndef EMACS
	EMACS = emacs
endif

.PHONY : test test-1 clean-elpa requirements env clean-env clean \
	print-deps travis-ci

test: elpa requirements
	make EMACS=${EMACS} CARTON=${CARTON} test-1

test-1:
	EMACS=${EMACS} ${CARTON} exec ${EMACS} -batch \
		-L . -l test-jedi.el -f ert-run-tests-batch-and-exit
	nosetests test_jediepcserver.py

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
	ls -d env/lib/python*/site-packages/*egg-info
	@echo "------------------------------------------------------------"

travis-ci: print-deps test
