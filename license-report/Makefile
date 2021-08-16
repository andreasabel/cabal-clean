# Andreas Abel, 2021-01-04, 2021-08-16
#
# Generate license-report using cabal-plan.

.PHONY: default index.md
default: index.html

index.md :
	cabal-plan license-report cabal-clean:exe:cabal-clean > $@

%.html : %.md
	pandoc -s -t html -o $@ $<

# EOF
