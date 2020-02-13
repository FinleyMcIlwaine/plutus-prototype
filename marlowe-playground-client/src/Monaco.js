/*eslint-env node*/
'use strict';

exports.getMonaco = function () {
    return global.monaco;
}

exports.create_ = function(monaco, nodeId) {
    return function() {
        monaco.editor.create(document.getElementById(nodeId), {
            value: [
              'function x() {',
              '\tconsole.log("Hello world!");',
              '}'
            ].join('\n'),
            language: 'javascript'
          });
    }
}
