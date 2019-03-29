import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse(
    '       CONFIGURATION SECTION.\n' +
      '       SOURCE-COMPUTER.\n' +
      '            IBM-3090.\n' +
      '       OBJECT-COMPUTER.\n' +
      '            IBM-3090.\n' +
      '       SPECIAL-NAMES.\n' +
      '       DECIMAL-POINT IS COMMA',
    {
      fromPath: false,
    },
  );

  expect(ast.nodes[0].type).toBe(Syntax.ConfigurationSection);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.ConfigurationSection).objectComputerParagraph).not.toBeNull();
  //   expect((ast.nodes[0] as Nodes.ConfigurationSection).objectComputerParagraph.objectComputerValue).toBe('IBM-3090');
  expect((ast.nodes[0] as Nodes.ConfigurationSection).sourceComputerParagraph).not.toBeNull();
  //   expect((ast.nodes[0] as Nodes.ConfigurationSection).sourceComputerParagraph.sourceComputerValue).toBe('IBM-3090');
  expect((ast.nodes[0] as Nodes.ConfigurationSection).specialNamesParagraph).not.toBeNull();
  //   expect((ast.nodes[0] as Nodes.ConfigurationSection).specialNamesParagraph.decimalPointClause).not.toBeNull();
});
