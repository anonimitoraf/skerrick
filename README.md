# Skerrik

REPL-driven development for Javascript.

TODO Installation instructions
Currently supported editors:
* VSCode
* Emacs

Inspired by (check them out!):
* SLIME: The Superior Lisp Interaction Mode for Emacs
* CIDER: The Clojure(Script) Interactive Development Environment that Rocks!

## Demos
* TODO

## Usage
* TODO

## Configuration
* `skerrick-result-overlay-face` - Face used to display evaluation results

## Bugs
* Requires and exports happen in the context of `skerrick/server/index.ts`. It should instead happen on the context of the passed in modulePath

## Roadmap
* Get it into MELPA
* Fill out the rest of this README
* Evaluation of the entry file on start up - might have to prompt the user and/or support the configuration via elisp
