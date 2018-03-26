.PHONY: build clean install uninstall reinstall test update-version release
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

update-version: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1)
update-version:
	@echo "Set version to $(VERSION)"
	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
	@sed -i 's/^\( *\)"ppx_protocol_conv" { >= ".*" }/\1"ppx_protocol_conv" { >= "$(VERSION)" }/' ppx_protocol_conv_*.opam

release: update-version
	./release.sh
