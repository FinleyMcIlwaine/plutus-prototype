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
    let monacoTokens = lexer.map(({ offset, type }) => ({
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

interface MooToken {
  col: number,
  line: number,
  type: string,
  value: string,
}

enum MarloweConstructor {
  Close,
  Pay,
  If,
  When,
  Let,
  AndObs,
  OrObs,
  NotObs,
  ChoseSomething,
  ValueGE,
  ValueGT,
  ValueLT,
  ValueLE,
  ValueEQ,
  TrueObs,
  FalseObs,
  AvailableMoney,
  Constant,
  NegValue,
  AddValue,
  SubValue,
  ChoiceValue,
  SlotIntervalStart,
  SlotIntervalEnd,
  UseValue,
  AccountId,
  Token,
  Account,
  Party,
  PK,
  Role,
  Bound,
  ValueId,
  Case,
  Deposit,
  Choice,
  Notify,
  ChoiceId,
}

function argumentList(constructorType: MarloweConstructor): Array<MarloweType> {
  switch (constructorType) {
    case MarloweConstructor.Pay:
      return [MarloweType.AccountId, MarloweType.Payee, MarloweType.Token, MarloweType.Value, MarloweType.Contract];
    case MarloweConstructor.If:
      return [MarloweType.Observation, MarloweType.Contract, MarloweType.Contract];
    case MarloweConstructor.When:
      return [MarloweType.Array, MarloweType.Number, MarloweType.Contract];
    case MarloweConstructor.Let:
      return [MarloweType.ValueId, MarloweType.Value, MarloweType.Contract];
    case MarloweConstructor.AndObs:
      return [MarloweType.Observation, MarloweType.Observation];
    case MarloweConstructor.OrObs:
      return [MarloweType.Observation, MarloweType.Observation];
    case MarloweConstructor.NotObs:
      return [MarloweType.Observation];
    case MarloweConstructor.ChoseSomething:
      return [MarloweType.ChoiceId];
    case MarloweConstructor.ValueGE:
      return [MarloweType.Value, MarloweType.Value];
    case MarloweConstructor.ValueGT:
      return [MarloweType.Value, MarloweType.Value];
    case MarloweConstructor.ValueLT:
      return [MarloweType.Value, MarloweType.Value];
    case MarloweConstructor.ValueLE:
      return [MarloweType.Value, MarloweType.Value];
    case MarloweConstructor.ValueEQ:
      return [MarloweType.Value, MarloweType.Value];
    case MarloweConstructor.AvailableMoney:
      return [MarloweType.AccountId, MarloweType.Token];
    case MarloweConstructor.Constant:
      return [MarloweType.Number];
    case MarloweConstructor.NegValue:
      return [MarloweType.Value];
    case MarloweConstructor.AddValue:
      return [MarloweType.Value, MarloweType.Value];
    case MarloweConstructor.SubValue:
      return [MarloweType.Value, MarloweType.Value];
    case MarloweConstructor.ChoiceValue:
      return [MarloweType.ChoiceId, MarloweType.Value];
    case MarloweConstructor.UseValue:
      return [MarloweType.ValueId];
    case MarloweConstructor.AccountId:
      return [MarloweType.Number, MarloweType.Party];
    case MarloweConstructor.Token:
      return [MarloweType.String, MarloweType.String];
    case MarloweConstructor.Account:
      return [MarloweType.AccountId];
    case MarloweConstructor.Party:
      return [MarloweType.Party];
    case MarloweConstructor.PK:
      return [MarloweType.String];
    case MarloweConstructor.Role:
      return [MarloweType.String];
    case MarloweConstructor.Bound:
      return [MarloweType.Number, MarloweType.Number];
    case MarloweConstructor.ValueId:
      return [MarloweType.String];
    case MarloweConstructor.Case:
      return [MarloweType.Action, MarloweType.Contract];
    case MarloweConstructor.Deposit:
      return [MarloweType.AccountId, MarloweType.Party, MarloweType.Token, MarloweType.Value];
    case MarloweConstructor.Choice:
      return [MarloweType.ChoiceId, MarloweType.Array];
    case MarloweConstructor.Notify:
      return [MarloweType.Observation];
    case MarloweConstructor.ChoiceId:
      return [MarloweType.String, MarloweType.Party];
    default:
      throw new Error("Tried to look up the arguments of type " + constructorType.toString() + " but it has none");
  }
}

function argumentType(constructorType: MarloweConstructor, argumentPosition: number): MarloweType {
  return argumentList(constructorType)[argumentPosition];
}

enum MarloweType {
  Contract,
  Observation,
  Value,
  AccountId,
  Token,
  Payee,
  Party,
  Bound,
  ValueId,
  Case,
  Action,
  ChoiceId,
  String,
  Number,
  Array,
}

/**
 * Find the index of the Constructor token for an argument and the position of the argument
 * 
 * For example in
 * 
 * `Deposit (AccountId 0 (Role "investor")) (Role "guarantor") (Token "" "") (Constant 1030)`
 * 
 * if we start at `Constant` then we want to return the index of `Deposit` and position 3
 * 
 * if we start at `"investor"` then we want to return the index of `AccountId` and position 1
 * @param tokens the tokens we are looking through
 * @param index the current index
 * @param depth the depth inside arguments we currently are
 * @param argPosition the position in the constructor the argument is
 */
function findConstructorIndex(tokens: Array<MooToken>, index: number, depth: number, argIndex: number): { index: number, argIndex: number } {
  if (index == 0) {
    // we are at the start so the constructor must be here
    return { index, argIndex };
  } else if (tokens[index].type == "lparen" && depth == 0) {
    // we are at the opening paren of the constructor so the constructor token is the next one
    return { index: index + 1, argIndex };
  } else if (tokens[index].type == "WS" && depth == 0) {
    // we are moving back 1 argument position
    return findConstructorIndex(tokens, index - 1, depth, argIndex + 1)
  } else if (tokens[index].type == "lparen") {
    // we are at the opening paren of an argument
    return findConstructorIndex(tokens, index - 1, depth - 1, argIndex);
  } else if (tokens[index].type == "rparen") {
    // we are at the closing paren of an argument so we record that in the depth
    return findConstructorIndex(tokens, index - 1, depth + 1, argIndex);
  } else {
    // nothing interesting, lets keep going
    return findConstructorIndex(tokens, index - 1, depth, argIndex);
  }
}

/**
 * Lookup the MarloweType of a token based on the structure of Marlowe
 * @param tokens 
 * @param currentTokenIndex 
 */
function lookupType(tokens: Array<MooToken>, currentTokenIndex: number): MarloweType {
  if (currentTokenIndex == 0) {
    return MarloweType.Contract;
  } else {
    let { index: constructorIndex, argIndex: argumentPosition } = findConstructorIndex(tokens, currentTokenIndex, 0, 0);
    let constructorType = MarloweConstructor[tokens[constructorIndex].value]
    return argumentType(constructorType, argumentPosition)
  }
}

/**
 * Return suggestions for a particular MarloweType
 * @param type 
 * @param range 
 */
function suggestion(type: MarloweType, range: monaco.IRange): monaco.languages.CompletionList {
  switch (type) {
    case MarloweType.Contract:
      return {
        suggestions: [
          {
            label: "Close",
            kind: monaco.languages.CompletionItemKind.Constructor,
            insertText: "Close",
            range
          },
          {
            label: "Pay",
            kind: monaco.languages.CompletionItemKind.Constructor,
            insertText: "Pay ?account_id",
            range
          },
        ]
      };
    case MarloweType.Observation:
      return { suggestions: [] };
    case MarloweType.Value:
      return { suggestions: [] };
    case MarloweType.AccountId:
      return { suggestions: [] };
    case MarloweType.Token:
      return { suggestions: [] };
    case MarloweType.Payee:
      return { suggestions: [] };
    case MarloweType.Party:
      return { suggestions: [] };
    case MarloweType.Bound:
      return { suggestions: [] };
    case MarloweType.ValueId:
      return { suggestions: [] };
    case MarloweType.Case:
      return { suggestions: [] };
    case MarloweType.Action:
      return { suggestions: [] };
    case MarloweType.ChoiceId:
      return { suggestions: [] };
    case MarloweType.String:
      return { suggestions: [] };
    case MarloweType.Number:
      return { suggestions: [] };
    case MarloweType.Array:
      return { suggestions: [] };
  }
}

export class MarloweCompletionItemProvider implements monaco.languages.CompletionItemProvider {

  provideCompletionItems(model: monaco.editor.ITextModel, position: monaco.Position, context: monaco.languages.CompletionContext, token: monaco.CancellationToken): monaco.languages.ProviderResult<monaco.languages.CompletionList> {
    const word = model.getWordAtPosition(position);
    const tokens: Array<MooToken> = Array.from(marloweLexer.reset(model.getValue()));
    // Find the token index of the word under our cursor
    const currentTokenIndex = tokens.findIndex(token => token.line == position.lineNumber && token.col == word.startColumn);
    const currentToken = tokens[currentTokenIndex];
    console.log(currentToken);
    const currentType = lookupType(tokens, currentTokenIndex);
    console.log(currentType);
    const range = {
      startLineNumber: currentToken.line,
      startColumn: currentToken.col,
      endLineNumber: currentToken.line,
      endColumn: currentToken.col + currentToken.value.length
    }
    return suggestion(currentType, range);
  }

}