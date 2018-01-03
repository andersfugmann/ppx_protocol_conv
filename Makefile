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
