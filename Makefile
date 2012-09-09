default: all
.PHONY: default all test git-submodule-incantations tidy clean

YAJL_PREFIX:=$(CURDIR)/upstream/local
YAJL_AR:=$(YAJL_PREFIX)/lib/libyajl_s.a

all: $(YAJL_AR) twt/ocaml+twt
	cd src && YAJL_PREFIX=$(YAJL_PREFIX) PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind yajl.cmxa yajl.cma

install: all
	ocamlfind remove yajl || true
	cd src/_build && ocamlfind install yajl $(YAJL_AR) libyajl_stubs.a yajl.a YAJL.cmi yajl.cma yajl.cmxa YAJL.mli ../META

doc: twt/ocaml+twt
	cd src && PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind yajl.docdir/index.html

test: install sample.json
	cd src && PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind -lflag yajl.cmxa test/test_yajl.native
	src/test_yajl.native $(CURDIR)/sample.json

$(YAJL_PREFIX)/lib/libyajl_s.a: .gitmodules upstream/CMakeLists.txt
	cd upstream && ./configure --prefix $(YAJL_PREFIX) && make install

upstream/CMakeLists.txt:
	$(MAKE) git-submodule-incantations

twt/ocaml+twt: .gitmodules twt/ocaml+twt.ml
	cd twt && make

twt/ocaml+twt.ml:
	$(MAKE) git-submodule-incantations

git-submodule-incantations:
	git submodule init
	git submodule sync
	git submodule update

sample.json:
	wget http://json-test-suite.googlecode.com/files/sample.zip
	unzip sample.zip sample.json
	rm sample.zip

tidy:
	cd src && ocamlbuild -clean

clean: tidy
	cd upstream && (make clean || true)
	cd twt && (make clean || true)
	rm -rf upstream/local
	rm -f sample.json
