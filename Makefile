default: all
.PHONY: default all install doc test extra install-extra doc-extra gh-pages git-submodule-incantations tidy clean

YAJL_PREFIX:=$(CURDIR)/upstream/local
YAJL_AR:=$(YAJL_PREFIX)/lib/libyajl_s.a
VERSION:=$(shell grep --only-matching [[:digit:]].[[:digit:]].[[:digit:]] src/META | head -n 1 | tr -d '\n')

all: $(YAJL_AR) twt/ocaml+twt
	cd src && YAJL_PREFIX=$(YAJL_PREFIX) PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind $(OCAMLBUILDFLAGS) yajl.cmxa yajl.cma

install: all
	ocamlfind remove yajl || true
	cd src/_build && ocamlfind install yajl $(YAJL_AR) libyajl_stubs.a yajl.a YAJL.cmi yajl.cma yajl.cmxa YAJL.mli ../META

doc: twt/ocaml+twt
	cd src && PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind yajl.docdir/index.html
	cp src/ocamldoc_style.css src/_build/yajl.docdir/style.css

test: install sample.json
	cd src && PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind $(OCAMLBUILDFLAGS) test/test_yajl.native
	src/test_yajl.native $(CURDIR)/sample.json

yajl_pretty: install
	cd src && PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind $(OCAMLBUILDFLAGS) test/yajl_pretty.native	

extra: twt/ocaml+twt
	ocamlfind query yajl || $(MAKE) install
	cd extra && PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind JSON.cmxa JSON.cma

install-extra: extra
	ocamlfind remove yajl-extra || true
	cd extra/_build && ocamlfind install yajl-extra JSON.a JSON.cmi JSON.cma JSON.cmxa JSON.mli ../META

doc-extra: twt/ocaml+twt
	cd extra && PATH=$(CURDIR)/twt:$(PATH) ocamlbuild -use-ocamlfind JSON.docdir/index.html
	cp src/ocamldoc_style.css extra/_build/JSON.docdir/style.css

$(YAJL_PREFIX)/lib/libyajl_s.a: .gitmodules upstream/CMakeLists.txt
	# --
	# work around yajl's ruby configure script; the following is all it does:
	mkdir -p upstream/build
	cd upstream/build && cmake -DCMAKE_INSTALL_PREFIX=$(YAJL_PREFIX) ..
	# --
	
	cd upstream/build && make install

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
	cd extra && ocamlbuild -clean

clean: tidy
	cd upstream && (make clean || true) && rm -rf build
	cd twt && (make clean || true)
	rm -rf upstream/local
	rm -f sample.json

uninstall:
	ocamlfind remove yajl
	ocamlfind remove yajl-extra

# generate tarball (GitHub's tarball feature doesn't work with submodules)
tarball: clean
	rm -rf upstream twt
	$(MAKE) git-submodule-incantations
	cd $(CURDIR)/.. && tar -czf yajl-ocaml-$(VERSION).tar.gz --exclude='*/.git/*' --exclude=".gitignore" --exclude=".DS_Store" yajl-ocaml
	tar tzf $(CURDIR)/../yajl-ocaml-$(VERSION).tar.gz
# note to self - subsequent steps to push tarball:
# git checkout tarball
# cp ../yajl-ocaml-x.x.x.tar.gz .
# git add yajl-ocaml-x.x.x.tar.gz
# git commit -m 'add yajl-ocaml-x.x.x.tar.gz'
# git push origin tarball

# generate ocamldoc and upload to gh-pages
gh-pages: clean
	rm -rf /tmp/yajl.docdir /tmp/extra.docdir
	$(MAKE) doc
	cp -r src/_build/yajl.docdir /tmp
	$(MAKE) doc-extra
	cp -r extra/_build/JSON.docdir /tmp
	git checkout gh-pages
	git pull origin gh-pages
	cp /tmp/yajl.docdir/* .
	cp /tmp/JSON.docdir/* extra
	git commit -am 'update ocamldoc documentation [via make gh-pages]'
	git push origin gh-pages
	git checkout master
