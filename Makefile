.PHONY: build clean install uninstall reinstall test
build:
	jbuilder build @install --dev

clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install

test: build
	jbuilder runtest --dev

bump_version:
	@if [ -z "$(VERSION)" ]; then echo "need to set VERSION"; exit 1; fi
	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
