import * as Nodes from '../nodes';

test('ProgramId constructor working correctly', () => {
  const node: Nodes.ProgramId = new Nodes.ProgramId(
    { startColumnTotal: 1, startColumnRelative: 2, startLine: 3 },
    'PGMNAME',
  );
  expect(node.startColumnTotal).toBe(1);
  expect(node.startColumnRelative).toBe(2);
  expect(node.startLine).toBe(3);
  expect(node.programIdValue).toBe('PGMNAME');
});

test('IdentificationDivisionContent constructor working correctly', () => {
  const node: Nodes.IdentificationDivisionContent = new Nodes.IdentificationDivisionContent(
    { startColumnTotal: 1, startColumnRelative: 2, startLine: 3 },
    'author',
    'installation',
    'dateWritten',
    'dateCompiled',
    'security',
  );
  expect(node.startColumnTotal).toBe(1);
  expect(node.startColumnRelative).toBe(2);
  expect(node.startLine).toBe(3);
  expect(node.author).toBe('author');
  expect(node.installation).toBe('installation');
  expect(node.dateWritten).toBe('dateWritten');
  expect(node.dateCompiled).toBe('dateCompiled');
  expect(node.security).toBe('security');
});
