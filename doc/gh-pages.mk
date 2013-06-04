REPO_URL = git@github.com:tkf/emacs-jedi.git

.PHONY: _gh-pages-assert-repo gh-pages-update gh-pages-push \
	gh-pages-clone gh-pages-pull

# Check if build/html is really a git repository.  Otherwise,
# committing files in there is pretty dangerous as it might goes into
# Jedi's master branch.
_gh-pages-assert-repo:
	test -d build/html/.git

gh-pages-clone:
	rm -rf build/html
	git clone --branch gh-pages $(REPO_URL) build/html

gh-pages-pull: _gh-pages-assert-repo
	cd build/html && git pull

gh-pages-update: _gh-pages-assert-repo clean html
	@echo "Update gh-pages"
	cd build/html/ && \
		git add . && \
		if [ -n "$$(git ls-files --deleted)" ]; then \
			git ls-files --deleted | xargs git rm; \
		fi && \
		git commit -m "Update"

gh-pages-push: _gh-pages-assert-repo
	cd build/html && git push -u origin gh-pages
