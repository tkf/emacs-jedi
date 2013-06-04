## Usage

# Usage 1:
# * Put this file in Sphinx's directory (where you find Makefile).
# * Add "include *.mk" to Sphinx's Makefile.

# Usage 2:
# * Run make like this:  make -f gh-pages.mk [TARGET]


## Configuration
REPO_URL = git@github.com:tkf/emacs-jedi.git
REPO_DIR = gh-pages
# $(DOC_VER) can be "released", "v1.0", etc.:
DOC_VER = latest
DOC_DIR = $(REPO_DIR)/$(DOC_VER)
DOC_PORT = 7563


## Misc variables

# To use this file from --file option, let's define variables in
# Sphinx's Makefile, if they are not defined:
ifndef BUILDDIR
BUILDDIR = build
endif


## Targets

.PHONY: _gh-pages-assert-repo gh-pages-update gh-pages-push \
	gh-pages-clone gh-pages-pull gh-pages-all \
	gh-pages-root gh-pages-root-commit

.NOTPARALLEL: gh-pages-all
gh-pages-all: gh-pages-pull gh-pages-update gh-pages-push

# Check if $(REPO_DIR) is really a git repository.  Otherwise,
# committing files in there is pretty dangerous as it might goes into
# REPO's master branch.
_gh-pages-assert-repo:
	test -d $(REPO_DIR)/.git

gh-pages-clone:
	rm -rf $(REPO_DIR)
	git clone --branch gh-pages $(REPO_URL) $(REPO_DIR)

gh-pages-pull: _gh-pages-assert-repo
	cd $(REPO_DIR) && git pull

gh-pages-update: _gh-pages-assert-repo
	$(MAKE) clean html
	@echo "Clean $(DOC_DIR)"
	test ! -d $(DOC_DIR)/.git
	rm -rf $(DOC_DIR)

	@echo "Copy files: $(BUILDDIR)/html -> $(DOC_DIR)"
	cp -r $(BUILDDIR)/html $(DOC_DIR)

	@echo "Update gh-pages"
	cd $(DOC_DIR) && \
		git add . && \
		if [ -n "$$(git ls-files --deleted .)" ]; then \
			git ls-files --deleted . | xargs git rm; \
		fi && \
		git commit -m "Update $(DOC_VER)"

gh-pages-push: _gh-pages-assert-repo
	cd $(REPO_DIR) && git push -u origin gh-pages


## Root

gh-pages-root-commit: gh-pages-root
	cd $(REPO_DIR) && \
		git add *.* .nojekyll .gitignore && \
		git commit -m "Update root page."

gh-pages-root: $(REPO_DIR)/index.html $(REPO_DIR)/.nojekyll

$(REPO_DIR)/index.html: gh-pages-index.html
	cp $< $@

$(REPO_DIR)/.nojekyll:
	touch $@


## Misc
gh-pages-serve:
	bash -c 'sleep 0.1s; python -m webbrowser http://localhost:$(DOC_PORT)' &
	cd $(REPO_DIR) && python -m SimpleHTTPServer $(DOC_PORT)
