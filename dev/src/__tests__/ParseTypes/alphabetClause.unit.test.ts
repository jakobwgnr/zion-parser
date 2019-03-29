import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('ALPHABET ALPHABET-NAME IS EBCDIC', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AlphabetClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetName).toBe('ALPHABET-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetType).toBe('EBCDIC');
});

test('Literal statement', () => {
  const ast: Ast = zionParser.parse('ALPHABET ALPHABET-NAME IS LITERAL-NAME', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AlphabetClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetName).toBe('ALPHABET-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetType).toBe('');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetLiterals[0].literal).toBe('LITERAL-NAME');
});

test('Literal THROUGH statement', () => {
  const ast: Ast = zionParser.parse('ALPHABET ALPHABET-NAME IS LITERAL-NAME THROUGH THROUGH-LITERAL', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AlphabetClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetName).toBe('ALPHABET-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetType).toBe('');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetLiterals[0].literal).toBe('LITERAL-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetLiterals[0].throughLiteral).toBe('THROUGH-LITERAL');
});

test('Literal THRU statement', () => {
  const ast: Ast = zionParser.parse('ALPHABET ALPHABET-NAME IS LITERAL-NAME THRU THRU-LITERAL', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AlphabetClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetName).toBe('ALPHABET-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetType).toBe('');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetLiterals[0].literal).toBe('LITERAL-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetLiterals[0].throughLiteral).toBe('THRU-LITERAL');
});

test(' ALSO statement', () => {
  const ast: Ast = zionParser.parse('ALPHABET ALPHABET-NAME IS LITERAL-NAME ALSO ALSO-LITERAL', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AlphabetClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetName).toBe('ALPHABET-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetType).toBe('');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetLiterals[0].literal).toBe('LITERAL-NAME');
  expect((ast.nodes[0] as Nodes.AlphabetClause).alphabetLiterals[0].alsoLiteral).toBe('ALSO-LITERAL');
});
