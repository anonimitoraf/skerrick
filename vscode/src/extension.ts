import * as vscode from 'vscode';
import { DecorationOptions, Range, TextEditor, TextEditorSelectionChangeKind } from 'vscode';
import axios from 'axios';

interface EvalResult {
  result: any;
  stdout: string;
  stderr: string;
}

const http = axios.create({});

const outputChannel = vscode.window.createOutputChannel('Skerrick');

const config = vscode.workspace.getConfiguration('skerrick');
const overlayTextMaxLength = config.get('resultMaxLengthBeforeTruncation') || 50;

let evalOverlay: DecorationOptions | undefined = undefined;
const overlayType = vscode.window.createTextEditorDecorationType({
  after: {
    margin: "0 0 0 0.5rem"
  },
  dark: { after: { border: "1px solid white" } },
  light: { after: { border: "1px solid black" } }
});

export function activate(context: vscode.ExtensionContext) {
  console.log('Skerrik activated');

  // Hide overlays on keyboard movement otherwise, the overlay moves too
  vscode.window.onDidChangeTextEditorSelection(event => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) return; // No open text editor
    // (Vim uses "Command events")
    if (event.kind === TextEditorSelectionChangeKind.Command
      || event.kind === TextEditorSelectionChangeKind.Keyboard) {
      hideOverlays(editor);
    }
  })

  const command = vscode.commands.registerCommand('skerrick.evalSelected', async () => {
    try {
      await evalCode()
    } catch (e) {
      vscode.window.showErrorMessage(`Failed to evaluate code: ${e}`);
    }
  });
  context.subscriptions.push(command);
}

async function evalCode () {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return; // No open text editor

  const { selection } = editor;

  const code = editor.document.getText(selection);
  const filePath = editor.document.fileName;

  outputChannel.appendLine(`[LOG] Evaluating code: ${code}`);
  const { result, stdout, stderr } = await http.post<EvalResult>('/eval', { code, modulePath: filePath }).then(r => r.data);
  outputChannel.appendLine(`[LOG] Result: ${result}`);
  const overlayText = `=> ${result === undefined ? 'undefined' : result.toString()}`;

  if (stdout) outputChannel.appendLine(`[STDOUT] ${stdout}`);
  if (stderr) outputChannel.appendLine(`[STDERR] ${stderr}`);
  outputChannel.show(true);

  if (!evalOverlay) {
    evalOverlay = makeOverlay(selection, overlayText);
  } else {
    evalOverlay = updateOverlay(evalOverlay, selection, overlayText);
  }
  editor.setDecorations(overlayType, [evalOverlay]);
}

function makeOverlay(selection: Range, text: string) {
  const decoration: DecorationOptions = {
    range: selection,
    renderOptions: { after: { contentText: text } }};
  return decoration;
}

function updateOverlay(overlay: DecorationOptions, newRange: Range, newText: string) {
  overlay.range = newRange;
  overlay.renderOptions!.after!.contentText = newText;
  return overlay;
}

function hideOverlays(editor: TextEditor) {
  editor.setDecorations(overlayType, []);
}

// this method is called when your extension is deactivated
export function deactivate() { }

function startServer () {

}
