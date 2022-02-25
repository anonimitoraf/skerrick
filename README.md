# Skerrick

> noun: **skerrick**  
>   the smallest bit.  
>   _"there's not a skerrick of food in the house"_

## :warning: IMPORTANT :warning:
THIS PROJECT IS IN ALPHA STATE!
I WOULD APPRECIATE IT IF YOU REPORT ANY BUGS/ISSUES YOU FIND. MAYBE EVEN CONTRIBUTE PATCHES/FIXES IF YOU HAVE THE TIME ;)

## Why should I care?

* REPL-driven development for Javascript
  * Programming in tiny increments - Apply patches to your running program without having to restart it
  * [Tell me more!](https://purelyfunctional.tv/lesson/what-is-repl-driven-development/)

Inspired by (check them out!):
* SLIME: The Superior Lisp Interaction Mode for Emacs
* CIDER: The Clojure(Script) Interactive Development Environment that Rocks!

## Emacs

### Demos
![Basic usage](/demos/basic.gif)
![Module support](/demos/modules.gif)

### Requirements
* `node`/`npm` installed and accessible by Emacs

### Installation
* `skerrick` isn't in MELPA yet. You can clone this repo and evaluate `skerrick.el` in the meantime or use `straight`. For example, if you use Doom:
```elisp
(package! skerrick
  :recipe (:host github :repo "anonimitoraf/skerrick"))
```

### Configuration
| Configuration | Desc | Default |
|:--|:--|:--|
| `skerrick-server-port` | Port to run the `skerrick` server on | 4321 |
| `skerrick-result-overlay-face` | Face used to display evaluation results | |

### Usage
| Command | Desc |
|:--|:--|
| `skerrick-install-or-upgrade-server-binary` | Needs to be run on the very first install of skerrick. Or when you want to upgrade. |
| `skerrick-start-server` | Starts the server. Note that your current buffer will be evaluated, so you probably want to run this command while being on your program's entry point file. |
| `skerrick-stop-server` | Stops the server. |
| `skerrick-eval-region` | Evaluates the selected region. Shows the eval results as an overlay. Stdout/stderr get written to the buffer `*skerrick-stdout-stderr*`. |

## VSCode
TODO

## Write a plug-in for your editor/IDE!
TODO
