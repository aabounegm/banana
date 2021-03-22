import { workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverPath = "D:\\Programs\\Haskell\\stack\\.local\\bin\\lsp-exe.exe";
  serverPath = workspace.getConfiguration('server').get('path', serverPath);

  let serverOptions: ServerOptions = {
    run: { command: serverPath, transport: TransportKind.stdio },
    debug: { command: serverPath, transport: TransportKind.stdio },
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'banana' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/*.banana')
    },
    diagnosticCollectionName: 'diag'
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'banana',
    'Banana',
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
