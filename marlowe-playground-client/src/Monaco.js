/*eslint-env node*/
'use strict';

exports.getMonaco = function () {
  return global.monaco;
}

exports.registerLanguage_ = function (monaco, language) {
  monaco.languages.register(language);
}

exports.setMonarchTokensProvider_ = function (monaco, languageId, languageDef) {
  monaco.languages.setMonarchTokensProvider(languageId, languageDef);
}

exports.create_ = function (monaco, nodeId, languageId, getMarkers) {
  monaco.editor.defineTheme('playground', require('../monacoMarlowe.js').marloweTheme);
  const editor = monaco.editor.create(nodeId, {
    value: [
      'Close'
    ].join('\n'),
    language: languageId,
    theme: 'playground',
  });
  editor.getModel().onDidChangeContent(event => {
    let model = editor.getModel();
    let contract = model.getValue();
    // parse and lint contract then set model markers
    const markers = getMarkers(contract);
    monaco.editor.setModelMarkers(model, "Owner - not sure what this is for", markers);
  });
  return editor;
}

exports.onDidChangeContent_ = function(editor, handler) {
  editor.getModel().onDidChangeContent(function(event) {
    handler(event)();
  });
}

exports.getModel_ = function (editor) {
  return editor.getModel();
}

exports.getValue_ = function (model) {
  return model.getValue();
}

exports.setValue_ = function (model, value) {
  return model.setValue(value);
}
exports.marloweTokensProvider = require('../monacoMarlowe.js').marloweTokensProvider;

exports.setTokensProvider_ = function (monaco, languageId, provider) {
  monaco.languages.setTokensProvider(languageId, provider);
}

exports.completionItemKind_ = function(name) {
  return monaco.languages.CompletionItemKind[name];
}

exports.markerSeverity_ = function(name) {
  return monaco.MarkerSeverity[name];
}

exports.registerCompletionItemProvider_ = function(monaco, languageId, suggestionsProvider, format, provideCodeActions) {
  let m = require('src/Marlowe/Monaco.ts')
  let uncurriedSuggestions = (stripParens, contract, range) => suggestionsProvider(stripParens)(contract)(range);
  let uncurriedProvideCodeActions = (a, b) => provideCodeActions(a)(b);
  let provider = new m.MarloweCompletionItemProvider(uncurriedSuggestions);
  let actionProvider = new m.MarloweCodeActionProvider(uncurriedProvideCodeActions);
  let formatter = new m.MarloweDocumentFormattingEditProvider(format);
  monaco.languages.registerCompletionItemProvider(languageId, provider);
  monaco.languages.registerCodeActionProvider(languageId, actionProvider);
  monaco.languages.registerDocumentFormattingEditProvider(languageId, formatter);
}

exports.setPosition_ = function(editor, position) {
  editor.setPosition(position);
}