#!/bin/bash -ex
#This script runs the unit tests inside a Travis CI worker. See also .travis.yml

export OPAM_VERSION=0.9.1

# install ocaml from apt
sudo apt-get update -qq
sudo apt-get install -qq ocaml

# install opam
curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz | tar xz
pushd opam-${OPAM_VERSION}
./configure
make > /dev/null
sudo make install
popd
opam init
eval `opam config -env`

# install packages from opam
opam install -q -y ocamlfind ounit

# compile & run tests
make test
