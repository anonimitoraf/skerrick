# Skerrick

> noun: **skerrick**  
>   the smallest bit.  
>   _"there's not a skerrick of food in the house"_

**REPL-driven development for NodeJS**
  * Programming in tiny increments - Apply patches to your running program without having to restart it
  * [Tell me more!](https://purelyfunctional.tv/lesson/what-is-repl-driven-development/)

Inspired by (check them out!):
  * SLIME: The Superior Lisp Interaction Mode for Emacs
  * CIDER: The Clojure(Script) Interactive Development Environment that Rocks!
  * Calva: integrated REPL powered environment for enjoyable and productive Clojure and ClojureScript development in Visual Studio Code

## :warning: DISCLAIMERS :warning:
* THIS PROJECT IS IN ALPHA STATE! I WOULD APPRECIATE IT IF YOU REPORT ANY BUGS/ISSUES YOU FIND. MAYBE EVEN CONTRIBUTE PATCHES/FIXES IF YOU HAVE THE TIME ;)
* The code is currently very messy and not well-written. I just wanted to get a working prototype out of the door as quickly as I could. I will be improving the implementation over time. Feel free to give me suggestions if you have any!

## VSCode

:sparkles: [Documentation](/vscode/README.md) :sparkles:

## Emacs

[[http://stable.melpa.org/#/skerrick][file:http://stable.melpa.org/packages/skerrick.svg]]  [[http://melpa.org/#/skerrick][file:http://melpa.org/packages/skerrick.svg]]

### Demos

#### Basic
![Basic usage](/demos/emacs/basic.gif)

#### Module support
![Module support](/demos/emacs/modules.gif)

### Requirements
* `node`/`npm` installed and accessible by Emacs

### Installation
`skerrick` is in MELPA

Or, if you're using Quelpa:
```
(quelpa '(skerrick :repo "anonimitoraf/skerrick" :fetcher github))

;; Needs to be run on the very first install of skerrick. Or when you want to upgrade.
(unless (equal (shell-command-to-string "type skerrick") "skerrick not found\n")
  (skerrick-install-or-upgrade-server-binary))

;; Should be run in a JS buffer; it is buffer specific.
;; (skerrick-start-server)

;; Now main function, entry point is:
;; M-x skerrick-eval-region
```

It may also be helpful to provide a quick keyboard shortcut. E.g., `C-x C-e` evaluates ELisp, so let's mimic that for JS buffers:
```
;; Evaluate a region, if any is selected; otherwise evaluate the current line.
(bind-key
 "C-x C-e"  (lambda ()
              (interactive)
              (if (use-region-p)
                  (skerrick-eval-region)
                (beginning-of-line)
                (set-mark-command nil)
                (end-of-line)
                (skerrick-eval-region)
                (pop-mark)))
 'js-mode-map)
```

### Configuration
| Configuration | Desc | Default |
|:--|:--|:--|
| `skerrick-server-port` | Port to run the `skerrick` server on | 4321 |
| `skerrick-result-overlay-face` | Face used to display evaluation results | |
| `skerrick-result-overlay-char-count-trunc` | Results with char count longer than this are truncated | 120 |
| `skerrick-pop-result-buffer-for-stdout` | Show result buffer if stdout is non-empty | t |
| `skerrick-pop-result-buffer-for-stderr` | Show result buffer if stderr is non-empty | t |

### Usage
| Command | Desc |
|:--|:--|
| `skerrick-install-or-upgrade-server-binary` | Needs to be run on the very first install of skerrick. Or when you want to upgrade. |
| `skerrick-start-server` | Starts the server. Note that your current buffer will be evaluated, so you probably want to run this command while being on your program's entry point file. |
| `skerrick-stop-server` | Stops the server. |
| `skerrick-eval-region` | Evaluates the selected region. Shows the eval result as an overlay. Stdout/stderr get written to the buffer `*skerrick-stdout-stderr*`. |

## Write a plug-in for your editor/IDE!

[![NPM](https://nodei.co/npm/skerrick.png)](https://nodei.co/npm/skerrick/)

* `npm install -g skerrick` - this installs the bin `skerrick`  

  Invoked like so: `skerrick PORT ENTRYPOINT_FULL_FILE_PATH`

* The REST protocol is as simple as it can be:
  * POST to `/eval` payloads of shape:
    ```json
      {
        "modulePath": "/full/path/to/file/of/code/to/eval",
        "code": "const x = 42; console.log(x); x + x;"
      }
    ```
  * Response shape:
    ```json
      {
        "result": "84",
        "stdout": "42",
        "stderr": ""
      }
    ```

* :rocket: Tell me about your plug-in so I can add it to this README! :rocket:
