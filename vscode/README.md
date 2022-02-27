# Skerrick

REPL-driven development for Javascript!

See https://github.com/anonimitoraf/skerrick for more info

## Features

* Make changes to your running program without having to restart it!  

  Is this like hot-reloading?  
  Yes, but (arguably) even better, because you choose which code to evaluate! With hot-reloading, your code gets re-evaluated for every change you make, whether you want to or not.
* Evaluate code and see the results inline - A feedback loop is faster than ever!

## Demos

Basic  
![Basic usage](/demos/vscode/basic.gif)

Module support  
![Module support](/demos/vscode/modules.gif)

## Configuration
| Configuration | Desc | Default |
|:--|:--|:--|
| `skerrick.serverPort` | Port to run the `skerrick` server on | 4321 |
| `skerrick.resultOverlayCharCountTrunc` | Results with char count longer than this are truncated | 120 |

## Usage
| Command | Desc |
|:--|:--|
| `Skerrick: Start server` | Starts the server. Note that your current buffer will be evaluated, so you probably want to run this command while being on your program's entry point file. |
| `Skerrick: Stop server` | Stops the server. |
| `Skerrick: Evaluate selected code` | Evaluates the selected region. Shows the eval result as an overlay. Result/stdout/stderr get written to the `Output > Skerrick` panel. |

## Known Issues

See https://github.com/anonimitoraf/skerrick/issues
