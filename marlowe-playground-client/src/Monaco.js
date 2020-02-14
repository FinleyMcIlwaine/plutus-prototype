/*eslint-env node*/
'use strict';

exports.getMonaco = function () {
  return global.monaco;
}

exports.registerLanguage_ = function(monaco, language) {
  monaco.languages.register(language);
}

exports.setMonarchTokensProvider_ = function(monaco, languageId, languageDef) {
  console.log(languageDef);
  monaco.languages.setMonarchTokensProvider(languageId, languageDef);
}

exports.create_ = function (monaco, nodeId, languageId) {
  monaco.editor.create(nodeId, {
    value: [
      'Close'
    ].join('\n'),
    language: languageId
  });
}
