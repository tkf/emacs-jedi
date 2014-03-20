mkdir /tmp/test/    # any location is fine
cd /tmp/test/
wget --no-check-certificate https://raw.github.com/tkf/emacs-jedi/master/doc/source/jedi-melpa.el
HOME=${PWD} emacs -Q -l jedi-melpa.el
