all: build runtest deb

build:
	erl -make

runtest: build
	cd ./deb/usr/share/test_task && ../../../../src/test.script state state_conversion_utils dispatcher resource_server

deb: build
	fakeroot dpkg-deb --build deb
	mv deb.deb ttask.deb
	chmod 744 ttask.deb

configure:
	sudo cp yaws.conf /etc/yaws/yaws.conf