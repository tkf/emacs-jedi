ENV = env
CARTON = carton
EMACS = emacs

test: elpa requirements
	EMACS=${EMACS} ${CARTON} exec ${EMACS} -batch \
		-L . -l test-jedi.el -f ert-run-tests-batch-and-exit

elpa:
	${CARTON} install

clean-elpa:
	rm -rf elpa

requirements: _env
	pip install --environment $(ENV) --requirement requirements.txt

_env: $(ENV)/bin/activate
$(ENV)/bin/activate: env

env:
	virtualenv $(ENV)

clean-env:
	rm -rf $(ENV)
