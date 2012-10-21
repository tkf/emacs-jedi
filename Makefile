ENV = env
CARTON = carton
ifndef EMACS
	EMACS = emacs
endif

.PHONY : test test-1 clean-elpa requirements env clean-env clean

test: elpa requirements
	make EMACS=${EMACS} CARTON=${CARTON} test-1

test-1:
	EMACS=${EMACS} ${CARTON} exec ${EMACS} -batch \
		-L . -l test-jedi.el -f ert-run-tests-batch-and-exit

elpa:
	${CARTON} install

clean-elpa:
	rm -rf elpa

requirements: env
	pip install --environment $(ENV) --requirement requirements.txt

env: $(ENV)/bin/activate
$(ENV)/bin/activate:
	virtualenv $(ENV)

clean-env:
	rm -rf $(ENV)

clean: clean-env clean-elpa
