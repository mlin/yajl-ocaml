default: all
.PHONY: default all test git-submodule-incantations tidy clean

YAJL_PREFIX:=$(CURDIR)/upstream/local
YAJL_AR:=$(YAJL_PREFIX)/lib/libyajl_s.a

all: $(YAJL_AR)
	cd src && YAJL_PREFIX=$(YAJL_PREFIX) ocamlbuild -use-ocamlfind yajl.cmxa yajl.cma

install: all
	ocamlfind remove yajl || true
	cd src/_build && ocamlfind install yajl $(YAJL_AR) libyajl_stubs.a yajl.a YAJL.cmi yajl.cma yajl.cmxa YAJL.mli ../META

test: install
	cd src && ocamlbuild -use-ocamlfind -lflag yajl.cmxa test/test_yajl.native
	src/test_yajl.native

$(YAJL_PREFIX)/lib/libyajl_s.a: upstream/CMakeLists.txt
	cd upstream && ./configure --prefix $(YAJL_PREFIX) && make install

upstream/CMakeLists.txt:
	$(MAKE) git-submodule-incantations
	cd upstream && git reset --hard HEAD

git-submodule-incantations:
	git submodule init
	git submodule sync
	git submodule update

tidy:
	cd src && ocamlbuild -clean

clean: tidy
	cd upstream && (make clean || true)
	rm -rf upstream/local
