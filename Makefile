project=$(shell basename `pwd`)

all: install 
solve:
	@stack solver --modify-stack-yaml

install: build
	@stack install

deps:
	@vim $(project).cabal

build:
	@stack build
	@echo -e "\e[1mBuild succesful\e[0m"

help:
	@echo "solve → if changed dependencies"
	@echo "run   → run compiled executabe"
	@echo "deps  → edit .cabal file"
	@echo "build → compile"
