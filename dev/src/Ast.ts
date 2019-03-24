import { Token } from 'zion-lexer';
import { ParseError } from './ErrorHandler/parse-error';
import { Node } from './nodes';

export class Ast {
  nodes: Node[];
  tokens: Token[];
  errors: ParseError[];
  constructor(nodes: Node[], tokens: Token[], errors: ParseError[]) {
    this.nodes = nodes;
    this.tokens = tokens;
    this.errors = errors;
  }
}
