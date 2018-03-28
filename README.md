# Tortoise

This is the repository containing source code and build systems for the Tortoise programming envirnoment.

## About

This is a (silly) attempt to create a programming language and envirnoment that's designed to enable constructionist teaching. Most of the language's design and philosphy is based on [the LOGO programming language](https://en.wikipedia.org/wiki/Logo_(programming_language))

## Building

This project uses Elm and Webpack. [Materialize-css](http://materializecss.com/) is also included to provide styling.

There are two npm run scripts;

```bash
$ npm run build
```

Will build the application into ```/dist```.

```bash
$ npm run client
```

Will start a live reloading dev server on ```localhost:3000```.


