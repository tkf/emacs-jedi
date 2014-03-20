mkdir /tmp/test/    # any location is fine
cd /tmp/test/
wget --no-check-certificate https://raw.github.com/tkf/emacs-jedi/master/doc/source/jedi-el-get.el
HOME=${PWD} emacs -Q --eval "(setq el-get-install-skip-emacswiki-recipes nil)" -l jedi-el-get.el
# HOME=${PWD} emacs -Q -l jedi-el-get.el
