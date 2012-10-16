ENV = env

serve:
	@$(ENV)/bin/python jediepcserver.py

requirements: requirements-deps
	pip install --environment $(ENV) --requirement requirements.txt

requirements-deps: $(ENV)/bin/activate
$(ENV)/bin/activate: env

env:
	virtualenv $(ENV)

clean-env:
	rm -rf $(ENV)
