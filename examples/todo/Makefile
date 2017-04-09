SHELL:=/bin/bash

# Avoid clashing with react-start-app build dir
BUILD_MAKE_DIR:=build-make

PROJECT_NAME:=glazier-react-todo

# externs in addition to all.js.externs
EXTERNS := $(BUILD_MAKE_DIR)/react-externs.js

# Run make with MINIFY=true to make minified js
ifeq ($(MINIFY),)
	project_source=$(BUILD_MAKE_DIR)/$(PROJECT_NAME).max.js
else
	project_source=$(BUILD_MAKE_DIR)/$(PROJECT_NAME).min.js
endif
export MINIFY

project_root:=$(shell stack path --project-root)

.PHONY: all env dir stack-clean clean check build install start install-prod deploy-prod

all: | install

# make all build directories
dir: $(BUILD_MAKE_DIR)

# create the ghcjs_project_out variable
env: $(project_root)/stack.ghcjs.yaml
	$(eval ghcjs_install_root := $(shell stack --stack-yaml $(project_root)/stack.ghcjs.yaml path --local-install-root))
	$(eval ghcjs_project_out := $(ghcjs_install_root)/bin/$(PROJECT_NAME).jsexe)

install: $(BUILD_MAKE_DIR)/$(PROJECT_NAME).js
	npm install

# Build prod optimized JS
install-prod:
	$(eval MINIFY := true)
	@$(MAKE) install
	npm run build

# Deploy to ghpages
deploy-prod: | install-prod
	npm run deploy

stack-clean:
	stack clean --stack-yaml $(project_root)/stack.ghcjs.yaml
	rm $(project_root)/stack.ghcjs.yaml

clean: | stack-clean
	rm -rf node_modules
	rm -rf $(BUILD_MAKE_DIR)

start: | install
	npm start

build: $(project_root)/stack.ghcjs.yaml
	stack build --stack-yaml $(project_root)/stack.ghcjs.yaml

$(project_root)/stack.ghcjs.yaml: $(project_root)/stack.yaml  $(project_root)/ghcjs.yaml
	cat $(project_root)/stack.yaml $(project_root)/ghcjs.yaml > $(project_root)/stack.ghcjs.yaml

$(BUILD_MAKE_DIR):
	mkdir -p $(BUILD_MAKE_DIR)

$(shell if [ ! $(BUILD_MAKE_DIR)/$(PROJECT_NAME).js -ef $(project_source) ]; then rm -rf $(BUILD_MAKE_DIR)/$(PROJECT_NAME).js; fi)
$(BUILD_MAKE_DIR)/$(PROJECT_NAME).js: $(project_source) | dir
	ln -sf `realpath $(project_source)` $(BUILD_MAKE_DIR)/$(PROJECT_NAME).js

$(BUILD_MAKE_DIR)/all.js: | env dir build
	ln -sf `realpath $(ghcjs_project_out)/all.js` $(BUILD_MAKE_DIR)/all.js

$(BUILD_MAKE_DIR)/all.js.externs: | env dir build
	ln -sf `realpath $(ghcjs_project_out)/all.js.externs` $(BUILD_MAKE_DIR)/all.js.externs

# add a nodeJS module wrapper http://edsko.net/2015/02/14/atom-haskell/
# Don't output to src as babel looks in that directory for js files to transpile.
# Babel will freeze trying to transpile ghcjs output.
# The nodeJS module wrapper will cause closure compiler fails with
# "ERROR - Closure dependency methods(goog.provide, goog.require, etc)
# must be called at file scope. "
# This step also removes the dummy calls to goog.provide and goog.require.
$(BUILD_MAKE_DIR)/$(PROJECT_NAME).max.js: $(BUILD_MAKE_DIR)/all.js haskell/jsbits/exports.header.js haskell/jsbits/exports.js | dir
	cat haskell/jsbits/exports.header.js \
		<(sed -e '/goog.provide.*/d;/goog.require.*/d' $(BUILD_MAKE_DIR)/all.js) \
		haskell/jsbits/exports.js \
		> $(BUILD_MAKE_DIR)/$(PROJECT_NAME).max.js

$(BUILD_MAKE_DIR)/$(PROJECT_NAME).min.js: $(BUILD_MAKE_DIR)/react-externs.js $(BUILD_MAKE_DIR)/all.js.externs $(BUILD_MAKE_DIR)/$(PROJECT_NAME).max.js | dir
	closure-compiler \
		--compilation_level=ADVANCED_OPTIMIZATIONS \
		--externs $(BUILD_MAKE_DIR)/all.js.externs \
		$(foreach extern, $(EXTERNS),--externs $(extern)) \
		--create_source_map build/$(PROJECT_NAME).min.js.map \
		--js $(BUILD_MAKE_DIR)/$(PROJECT_NAME).max.js \
		--js_output_file $(BUILD_MAKE_DIR)/$(PROJECT_NAME).min.js

$(EXTERNS):
	wget https://raw.githubusercontent.com/steida/react-externs/master/externs.js -O $(BUILD_MAKE_DIR)/react-externs.js
