.PHONY: build clean install uninstall reinstall
build:
	jbuilder build @install --dev

clean:
	jbuilder clean

install:
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install
