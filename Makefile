test:
	faketime '2024-01-01 12:00:00' \
	emacs --quick --batch --directory=$(CURDIR) \
		--load giornata-tests.el \
		--funcall ert-run-tests-batch-and-exit

clean:
	git clean -Xdf

FORCE:
