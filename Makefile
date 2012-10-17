ENV = env

serve:
	@$(ENV)/bin/python jediepcserver.py

requirements: _env
	pip install --environment $(ENV) --requirement requirements.txt

_env: $(ENV)/bin/activate
$(ENV)/bin/activate: env

env:
	virtualenv $(ENV)

clean-env:
	rm -rf $(ENV)
