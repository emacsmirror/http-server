EMACS ?= emacs

EL_FILES = $(filter-out %-tests.el, $(wildcard *.el))

# Install plz directly from the git repo instead of GNU ELPA, because of this unreleased
# commit [1], which allows arbitrary methods such as TRACE.
#
# [1] https://github.com/alphapapa/plz.el/commit/a91994aae91e4db96062abc7cbabbccf8e09cd16
DEPS = websocket (plz :url \"https://github.com/alphapapa/plz.el\")

INIT_PACKAGES="(progn \
  (require 'package) \
  (package-initialize) \
  (unless package-archive-contents (package-refresh-contents)) \
  (dolist (pkg '(${DEPS})) \
    (if-let* ((sym (and (consp pkg) (car pkg)))) \
        (unless (package-installed-p sym) \
          (package-vc-install pkg)) \
      (unless (package-installed-p pkg) \
        (package-install pkg)))))"

.PHONY: all docs clean test lint

all: docs

docs: docs/http-server.info docs/dir

docs/http-server.info: docs/http-server.org
	cd docs && $(EMACS) --batch \
		--eval "(require 'ox-texinfo)" \
		http-server.org \
		--funcall org-texinfo-export-to-info

docs/dir: docs/http-server.info
	install-info docs/http-server.info docs/dir

lint: byte-compile checkdoc package-lint

package-lint: DEPS = (package-lint :url \"https://github.com/purcell/package-lint\")
package-lint:
	$(EMACS) --batch \
		--eval $(INIT_PACKAGES) \
		-f package-lint-batch-and-exit \
		http-server.el

byte-compile:
	$(EMACS) --batch \
		--eval $(INIT_PACKAGES) \
		--eval "(add-to-list 'load-path default-directory)" \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_FILES)

checkdoc:
	$(EMACS) --batch \
		--eval "(require 'checkdoc)" \
		--eval "(let ((n 0)) \
		           (advice-add (quote display-warning) :before \
		                       (lambda (type &rest _) (when (eq type (quote emacs)) (setq n (1+ n)))) \
		                       (quote ((name . count-checkdoc-warnings)))) \
		           (mapc (function checkdoc-file) (list $(patsubst %,\"%\",$(EL_FILES)))) \
		           (when (> n 0) (message \"checkdoc: found docstring issues (see warnings above)\") (kill-emacs 1)))"

TEST_SELECTOR ?= t
test: lint
	$(EMACS) --batch \
		--eval $(INIT_PACKAGES) \
		-l http-server.el \
		-l http-server-ws.el \
		-l http-server-tests.el \
		-l http-server-ws-tests.el \
		--eval "(ert-run-tests-batch-and-exit '${TEST_SELECTOR})"

clean:
	rm -f docs/http-server.info docs/http-server.texi docs/dir *.elc
