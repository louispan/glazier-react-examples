# Glazier.React TodoMVC example

This is a fully featured TodoMVC in in Haskell and ReactJS using the [glazier-react](https://github.com/louispan/glazier-react) library.

For a live demo, see https://louispan.github.io/glazier-react-examples/

## dependencies
* [stack](https://haskell-lang.org/get-started)
* [ghcjs](https://docs.haskellstack.org/en/stable/ghcjs/)
* [node](https://nodejs.org)
 - This gives the npm package manager.
 - node is required for ghcjs anyway.
* [closure-compiler](https://developers.google.com/closure/compiler/)
 - NB. The makefile assumes it is called closure-compiler.
* [glazier-react-examples repo](https://github.com/louispan/glazier-react-examples)

## installation
```
cd examples/todo
../../stackjs build && make start
```
# for minified version
```
cd examples/todo
MINIFY=1 bash -c '../../stackjs build && make start'
```

## misc
The examples/todo was bootstrapped using the skeleton from
https://facebook.github.io/react/docs/installation.html, by running:
```
npm install -g create-react-app
create-react-app todo
```