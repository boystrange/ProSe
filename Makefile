YAML    = stack.yaml
TESTS   = $(wildcard examples/*.pi)
ERRORS  = $(wildcard errors/*.pi)
DEST    =
STACK   = stack --stack-yaml $(YAML)
NULL    =
SOURCES = \
  Atoms.hs \
  Checker.hs \
  Common.hs \
  Exceptions.hs \
  Formula.hs \
  Node.hs \
  Predicate.hs \
  Process.hs \
  Relation.hs \
  Render.hs \
  Resolver.hs \
  Interpreter.hs \
  Tree.hs \
  Type.hs \
  $(NULL)

all:
	@$(STACK) build

watch:
	@$(STACK) build --file-watch

install:
	@$(STACK) install

dist:
	@cabal sdist

sync:
	@make -C html
	@scp html/*.* $(DEST)
	@scp dist/*.tar.gz $(DEST)

info:
	@$(STACK) exec happy -- -i src/Parser.y

%.check_ok:
	@stack run $(@:%.check_ok=%) || echo

check_examples:
	@echo
	@echo "SCRIPTS THAT MUST PASS"
	@echo "–————————————————————–"
	@for i in $(TESTS); do make -s $$i.check_ok; done

%.check:
	@faircheck --log $(@:%.check=%) || echo

check_errors:
	@echo
	@echo "SCRIPTS THAT MUST FAIL"
	@echo "–————————————————————–"
	@for i in $(ERRORS); do make -s $$i.check; done

update_license:
	for hs in `find src -name "*.hs"`; do \
		TEMP=`mktemp`; \
		cp LICENSE.hs $$TEMP; \
		tail -n +17 <$$hs >>$$TEMP; \
		mv $$TEMP $$hs; \
	done

check: check_examples check_errors
	@echo

.PHONY: dist clean check check_examples check_errors docs

clean:
	@$(STACK) clean
	@rm -f `$(STACK) path --local-bin`/faircheck
