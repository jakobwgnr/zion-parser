import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('objectComputerParagraph working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE 123 WORDS\n' +
        'PROGRAM COLLATING SEQUENCE IS SEQ\n' +
        'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('SEQ');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Expect numeric memory size - throw error if not', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE ABC WORDS\n' +
        'PROGRAM COLLATING SEQUENCE IS SEQ\n' +
        'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(1);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('SEQ');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Expect identifier sequence - throw error if not', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE 123 WORDS\n' +
        'PROGRAM COLLATING SEQUENCE IS 123\n' +
        'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(1);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Expect identifier segmentLimit - throw error if not', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE 123 WORDS\n' +
        'PROGRAM COLLATING SEQUENCE IS SEQ\n' +
        'SEGMENT-LIMIT IS 123.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBeGreaterThan(0);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('SEQ');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('');
  });

  test('Full statement - without "PROGRAM" KEYWORD', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE 123 WORDS\n' + 'COLLATING SEQUENCE IS SEQ\n' + 'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('SEQ');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Expect numeric memory size - throw error if not - without "PROGRAM" KEYWORD', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE ABC WORDS\n' + 'COLLATING SEQUENCE IS SEQ\n' + 'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(1);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('SEQ');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Expect identifier sequence - throw error if not - without "PROGRAM" KEYWORD', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE 123 WORDS\n' + 'COLLATING SEQUENCE IS 123\n' + 'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(1);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Full statement - without "PROGRAM" KEYWORD - without "COLLATING" KEYWORD', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE 123 WORDS\n' + 'SEQUENCE IS SEQ\n' + 'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('SEQ');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Expect numeric memory size - throw error if not - without "PROGRAM" KEYWORD  - without "COLLATING" KEYWORD', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE ABC WORDS\n' + 'SEQUENCE IS SEQ\n' + 'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(1);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('SEQ');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('Expect identifier sequence - throw error if not - without "PROGRAM" KEYWORD - without "COLLATING" KEYWORD', () => {
    const ast: Ast = zionParser.parse(
      'OBJECT-COMPUTER. COMNAME MEMORY SIZE 123 WORDS\n' + 'SEQUENCE IS 123\n' + 'SEGMENT-LIMIT IS LIM.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(1);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('COMNAME');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('LIM');
  });

  test('minimum statement', () => {
    const ast: Ast = zionParser.parse('OBJECT-COMPUTER. IBM-123.', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.ObjectComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).objectComputerValue).toBe('IBM-123');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).memorySizeValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).sequenceValue).toBe('');
    expect((ast.nodes[0] as Nodes.ObjectComputerParagraph).segmentLimitValue).toBe('');
  });
});
