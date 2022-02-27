import * as vscode from 'vscode';
import { DecorationOptions, Range, TextEditor, TextEditorSelectionChangeKind } from 'vscode';
import axios, { AxiosInstance } from 'axios';
import * as skerrickServer from 'skerrick';

interface EvalResult {
  result: any;
  stdout: string;
  stderr: string;
}

let http: AxiosInstance;

const outputChannel = vscode.window.createOutputChannel('Skerrick');

const config = vscode.workspace.getConfiguration('skerrick');
const configResultOverlayCharCountTrunc: number = config.get('resultOverlayCharCountTrunc') || 120;
const configserverPort: number = config.get('serverPort') || 4321;

let evalOverlay: DecorationOptions | undefined = undefined;
const overlayType = vscode.window.createTextEditorDecorationType({
  after: {
    margin: "0 0 0 0.5rem"
  },
  dark: { after: { border: "0.5px solid #808080" } },
  light: { after: { border: "0.5px solid #c5c5c5" } }
});

export function activate(context: vscode.ExtensionContext) {
  console.log('Skerrick activated');

  http = axios.create({ baseURL: 'http://localhost:' + configserverPort });

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

  const evalCommand = vscode.commands.registerCommand('skerrick.evalSelected', async () => {
    try {
      await evalCode()
    } catch (e) {
      vscode.window.showErrorMessage(`Failed to evaluate code: ${e}`);
    }
  });
  const startServerCommand = vscode.commands.registerCommand('skerrick.startServer', startServer);
  const stopServerCommand = vscode.commands.registerCommand('skerrick.stopServer', stopServer);

  [evalCommand, startServerCommand, stopServerCommand].forEach(cmd => context.subscriptions.push(cmd));
}

async function evalCode () {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return; // No open text editor

  const { selection } = editor;

  const code = editor.document.getText(selection);
  const filePath = editor.document.fileName;

  outputChannel.appendLine(`[EVAL] ${code}`);
  const { result, stdout, stderr } = await http.post<EvalResult>('/eval', { code, modulePath: filePath }).then(r => r.data);
  outputChannel.appendLine(`[RESULT] ${result}`);

  let overlayText: string;
  if (result === undefined) {
    overlayText = '=> undefined';
  } else if (result.toString().length > configResultOverlayCharCountTrunc) {
    overlayText = '=> ' + result.toString().substring(0, configResultOverlayCharCountTrunc) + '... (result truncated, see Output > Skerrick  panel)';
    outputChannel.show(true);
  } else {
    overlayText = '=> ' + result.toString();
  }

  if (stdout) outputChannel.appendLine(`[STDOUT] ${stdout}`);
  if (stderr) outputChannel.appendLine(`[STDERR] ${stderr}`);
  // TODO Maybe make this configurable
  if (stdout?.trim() || stderr?.trim()) {
    outputChannel.show(true);
  }

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

let serverInstance: (() => void) | undefined;

function startServer () {
  if (serverInstance) {
    outputChannel.appendLine(`[NOTICE]: Attempted to start skerrick server but it's already running. No op...`);
  }

  const editor = vscode.window.activeTextEditor;
  if (!editor) return; // No open text editor

  const filePath = editor.document.fileName;
  serverInstance = skerrickServer.serve(configserverPort, filePath, true);
  outputChannel.appendLine(`[NOTICE]: Started skerrick server on port ${configserverPort}`);
}

function stopServer() {
  if (!serverInstance) {
    outputChannel.appendLine(`[NOTICE]: Attempted to stop skerrick server but there is no running instance. No op...`);
  } else {
    serverInstance();
    serverInstance = undefined;
    outputChannel.appendLine(`[NOTICE]: Stopped skerrick server on port ${configserverPort}`);
  }
}

// this method is called when your extension is deactivated
export function deactivate() {
  stopServer();
}
