import * as moo from 'moo';
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';

const marloweLexer = moo.compile({
  WS: /[ \t]+/,
  number: /0|-?[1-9][0-9]*/,
  string: { match: /"(?:\\["\\]|[^\n"\\])*"/, value: x => x.slice(1, -1) },
  comma: ',',
  lparen: '(',
  rparen: ')',
  lsquare: '[',
  rsquare: ']',
  hole: /\?[a-zA-Z0-9_-]+/,
  CONSTRUCTORS: {
    match: /[A-Z][A-Za-z]+/, type: moo.keywords({
      CONTRACT: ['Close', 'Pay', 'If', 'When', 'Let'],
      OBSERVATION: [
        'AndObs',
        'OrObs',
        'NotObs',
        'ChoseSomething',
        'ValueGE',
        'ValueGT',
        'ValueLT',
        'ValueLE',
        'ValueEQ',
        'TrueObs',
        'FalseObs',
      ],
      VALUE: [
        'AvailableMoney',
        'Constant',
        'NegValue',
        'AddValue',
        'SubValue',
        'ChoiceValue',
        'SlotIntervalStart',
        'SlotIntervalEnd',
        'UseValue',
      ],
      ACCOUNT_ID: ['AccountId'],
      TOKEN: ['Token'],
      PAYEE: ['Account', 'Party'],
      PARTY: ['PK', 'Role'],
      BOUND: ['Bound'],
      VALUE_ID: ['ValueId'],
      CASE: ['Case'],
      ACTION: ['Deposit', 'Choice', 'Notify'],
      CHOICE_ID: ['ChoiceId'],
    })
  },
  NL: { match: /\n/, lineBreaks: true },
  myError: { match: /[\$?`]/, error: true },
});

export class State implements monaco.languages.IState {
  lexer: any
  constructor(lexer) {
    this.lexer = lexer;
  }

  equals(other) {
    return (other === this || other.lexer === this.lexer);
  }

  clone() {
    return new State(this.lexer);
  }
}

export const marloweTokensProvider: monaco.languages.TokensProvider = {
  getInitialState: function () {
    return new State(marloweLexer);
  },
  tokenize: function (line, startState: State) {
    let lexer = startState.lexer;
    lexer.reset(line);
    let monacoTokens = lexer.map(({offset, type}) => ({
      startIndex: offset,
      scopes: type,
    }));
    return {
      endState: new State(lexer),
      tokens: monacoTokens,
    }
  }
}

export const marloweTheme = {
  base: 'vs',
  inherit: true,
  rules: [
    { token: "hole", foreground: '0000ff', fontStyle: 'italic' },
    { token: "CONTRACT", foreground: '0000ee', fontStyle: 'italic' },
  ],
}