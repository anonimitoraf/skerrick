# JIVE
**J**avascript **I**nteractive de**V**elopment **Environment** (I could've gone for JIDE but doesn't sound as nice and is also not an actual word ;))

REPL-driven development for Javascript in Emacs. Powered by [Replete](https://github.com/jamesdiacono/Replete)

Inspired by (check them out!):
* SLIME: The Superior Lisp Interaction Mode for Emacs
* CIDER: The Clojure(Script) Interactive Development Environment that Rocks!

WORK IN-PROGRESS

## Demos
* [Basic usage](https://github.com/anonimitoraf/jive/blob/main/demos/jive-demo-1.gif)

## Usage
* `jive-start`
* Select the code you want to evaluate then `jive-eval-region`

## Configuration
* `jive--replete-js-path` (required) - Path to replete.js. See https://github.com/jamesdiacono/Replete/blob/trunk/replete.js

## TODO
* Proper popups
* Support for `require` (currently only `import` seems supported by Replete)
* Support for Typescript in Replete. I get an acornJS error when I try to eval TS code
* Support for browser JS
* etc
