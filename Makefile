project=$(shell basename `pwd`)

all: install 
solve:
	@stack solver --modify-stack-yaml

install: build
	@stack install
	@echo -e '\033[1;32m\e[1mInstallation succesful\e[0m'

deps:
	@vim $(project).cabal

build:
	@stack build
	@echo -e '\e[1mBuild succesful\e[0m'

help:
	@echo 'solve   → if changed dependencies'
	@echo 'install → install program (it will be in $$PATH)'
	@echo 'deps    → edit .cabal file'
	@echo 'build   → compile'
