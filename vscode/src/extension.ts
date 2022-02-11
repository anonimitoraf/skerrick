// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { DecorationOptions, Range } from 'vscode';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
  // Use the console to output diagnostic information (console.log) and errors (console.error)
  // This line of code will only be executed once when your extension is activated
  console.log('Congratulations, your extension "jive" is now active!');

  // The command has been defined in the package.json file
  // Now provide the implementation of the command with registerCommand
  // The commandId parameter must match the command field in package.json
  let disposable = vscode.commands.registerCommand('jive.helloWorld', evaluateCode);

  context.subscriptions.push(disposable);
}

function evaluateCode () {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return; // No open text editor

  const { selection } = editor;

  const decorationType = vscode.window.createTextEditorDecorationType({});
  editor.setDecorations(decorationType, [makeOverlay(selection, "=> blah")]);
}

function makeOverlay(selection: Range, text: string) {
  const decoration: DecorationOptions = {
    range: selection,
    renderOptions: {
      after: {
        contentText: text,
        margin: "0 0 0 0.5rem",
        border: "1px solid white"
      }
    }
  };
  return decoration;
}

// this method is called when your extension is deactivated
export function deactivate() { }
