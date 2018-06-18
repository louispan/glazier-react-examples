SHELL:=/bin/bash

PROJECT_NAME:=glazier-react-todo

include ghcjs.mk

EXTERNS += $(BUILD_DIR)/react-externs.js

.PHONY: clean install start

.PHONY: clean install start install-prod deploy-prod

clean::
	rm -rf node_modules

install: build
	npm install

start: | install
	npm start

# Build prod optimized JS
install-prod:
	$(eval MINIFY := true)
	@$(MAKE) install
	npm run build

# Deploy to ghpages
deploy-prod: | install-prod
	npm run deploy

$(BUILD_DIR)/react-externs.js: | $(BUILD_DIR)
	wget https://raw.githubusercontent.com/Jollor/react-externs/master/externs.js -O $(BUILD_DIR)/react-externs.js
