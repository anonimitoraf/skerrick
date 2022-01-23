# JIVE
**J**avascript **I**nteractive de**V**elopment **E**nvironment (I could've gone for JIDE but doesn't sound as nice and is also not an actual word ;))

REPL-driven development for NodeJS in Emacs.

Inspired by (check them out!):
* SLIME: The Superior Lisp Interaction Mode for Emacs
* CIDER: The Clojure(Script) Interactive Development Environment that Rocks!

WORK IN-PROGRESS

## Demos
* TODO

## Usage
* TODO

## Configuration
* `jive-result-overlay-face` - Face used to display evaluation results

## Bugs
* Requires and exports happen in the context of `jive/server/index.ts`. It should instead happen on the context of the passed in modulePath

## Roadmap
* Get it into MELPA
* Fill out the rest of this README
* Evaluation of the entry file on start up - might have to prompt the user and/or support the configuration via elisp
