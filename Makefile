all: build runtest deb

clean:
	find . -name *.beam -exec rm -f {} \;
	find . -name *.deb -exec rm -f {} \;

docker: deb
	cp ./ttask.deb ./container_sample/ttask.deb
	sudo docker build ./container_sample

build:
	erl -make

runtest: build
	cd ./deb/usr/share/test_task && ../../../../src/test.script state state_conversion_utils dispatcher resource_server

deb: build
	fakeroot dpkg-deb --build deb
	mv deb.deb ttask.deb
	chmod 744 ttask.deb

configure:
	sudo cp ./container_sample/yaws.conf /etc/yaws/yaws.conf