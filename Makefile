.PHONY: build clean install uninstall reinstall test update-version release doc
build:
	dune build @install

clean:
	dune clean

install:
	dune build @install
	dune install

uninstall:
	dune uninstall

reinstall: uninstall install

test: build
	dune runtest

update-version: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1)
update-version:
	@echo "Set version to $(VERSION)"
	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
	@sed -i 's/^\( *\)"ppx_protocol_conv" {[ ]*>= ".*"[ ]*}/\1"ppx_protocol_conv" {>= "$(VERSION)"}/' ppx_protocol_conv_*.opam

release:
	opam publish

doc:
	dune build @doc

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp  -r _build/default/_doc/_html/* .gh-pages
	git -C .gh-pages add .
	git -C .gh-pages config user.email 'docs@ppx_protocol_conv'
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

debug:
	dumpast type.ml
	_build/default/.ppx/ppx_protocol_conv+ppx_sexp_conv+ppx_driver.runner/ppx.exe type.ml
