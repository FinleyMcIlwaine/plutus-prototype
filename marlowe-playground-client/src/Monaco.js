/*eslint-env node*/
'use strict';

const x = require('./Marlowe/Monaco.ts');

exports.getMonaco = function () {
  return global.monaco;
}

exports.registerLanguage_ = function (monaco, language) {
  monaco.languages.register(language);
}

exports.setMonarchTokensProvider_ = function (monaco, languageId, languageDef) {
  console.log(languageDef);
  monaco.languages.setMonarchTokensProvider(languageId, languageDef);
}

exports.create_ = function (monaco, nodeId, languageId) {
  monaco.editor.defineTheme('playground', require('../monacoMarlowe.js').marloweTheme);
  monaco.editor.create(nodeId, {
    value: [
      'Close'
    ].join('\n'),
    language: languageId,
    theme: 'playground',
  });
}

exports.getModel_ = function (monaco) {
  return monaco.editor.getModel("inmemory://model/1");
}

exports.marloweTokensProvider = require('../monacoMarlowe.js').marloweTokensProvider;

exports.setTokensProvider_ = function (monaco, languageId, provider) {
  monaco.languages.setTokensProvider(languageId, provider);
}

exports.completionItemKind_ = function(name) {
  return monaco.languages.CompletionItemKind.Constructor;
}

exports.registerCompletionItemProvider_ = function(monaco, languageId, suggestionsProvider) {
  let m = require('./Marlowe/Monaco.ts')
  let uncurried = (stripParens, contract, range) => suggestionsProvider(stripParens)(contract)(range);
  let provider = new m.MarloweCompletionItemProvider(uncurried);
  monaco.languages.registerCompletionItemProvider(languageId, provider);
}