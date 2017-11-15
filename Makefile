.PHONY: build clean install uninstall reinstall test
build:
	jbuilder build @install --dev

clean:
	jbuilder clean

install:
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install

test:
	jbuilder runtest --dev
