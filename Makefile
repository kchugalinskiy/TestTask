all: build

build:
	erl -make

runtest: build
	cd ./deb/usr/share/test_task && ../../../../src/test.script state state_conversion_utils dispatcher resource_server

deb:
	echo "Not implemented"