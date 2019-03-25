import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('parseIdentificationDivisionContent working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse(
      'AUTHOR. IMPLEMENTING CORP.\n' +
        'INSTALLATION. THE INSTALL.\n' +
        'DATE-WRITTEN. 2019-03-25 1995-01-01.\n' +
        'DATE-COMPILED. 1995-01-01 2019-03-25.\n' +
        'SECURITY. THE SEC.',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.IdentificationDivisionContent);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.IdentificationDivisionContent).author).toBe('IMPLEMENTING CORP');
    expect((ast.nodes[0] as Nodes.IdentificationDivisionContent).installation).toBe('THE INSTALL');
    expect((ast.nodes[0] as Nodes.IdentificationDivisionContent).dateWritten).toBe('2019-03-25 1995-01-01');
    expect((ast.nodes[0] as Nodes.IdentificationDivisionContent).dateCompiled).toBe('1995-01-01 2019-03-25');
    // expect((ast.nodes[0] as Nodes.IdentificationDivisionContent).security).toBe('THE SEC');
  });

  test('error statement', () => {
    const ast: Ast = zionParser.parse(
      'AUTHOR. .\n' + 'INSTALLATION. .\n' + 'DATE-WRITTEN. .\n' + 'DATE-COMPILED. .\n' + 'SECURITY. .',
      {
        fromPath: false,
      },
    );
    expect(ast.nodes[0].type).toBe(Syntax.IdentificationDivisionContent);
    expect(ast.errors.length).toBe(5);
  });

  test('minimal statement', () => {
    const ast: Ast = zionParser.parse('AUTHOR IMPLEMENTING CORP\n', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.IdentificationDivisionContent);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.IdentificationDivisionContent).author).toBe('IMPLEMENTING CORP');
  });
});
