# JIVE
**J**avascript **I**nteractive de**V**elopment **E**nvironment (I could've gone for JIDE but doesn't sound as nice and is also not an actual word ;))

REPL-driven development for Javascript in Emacs. Powered by [Replete](https://github.com/jamesdiacono/Replete)

Inspired by (check them out!):
* SLIME: The Superior Lisp Interaction Mode for Emacs
* CIDER: The Clojure(Script) Interactive Development Environment that Rocks!

WORK IN-PROGRESS

## Demos
* [Basic usage](https://github.com/anonimitoraf/jive/blob/main/demos/jive-demo-1.gif)

## Usage
* `jive-start`
* Select the code you want to evaluate either:
    * `jive-node-eval-region`
    * `jive-deno-eval-region`

## Configuration
* `jive--replete-js-path` (required) - Path to replete.js. See https://github.com/jamesdiacono/Replete/blob/trunk/replete.js
* `jive-result-overlay-face` - Face used to display evaluation results

## TODO - Bugs
* Support for Typescript in Replete. I get an acornJS error when I try to eval TS code
* Support for browser JS

## TODO - Features
* Evaluation of the entry file on start up - might have to prompt the user and/or support the configuration via elisp
* A dedicated buffer for evaluating strings manually (like CIDER's)
* Confirm with user that they want to stop JIVE
