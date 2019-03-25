import { Node } from '../nodes';
import * as Nodes from '../nodes';

test('Node Constructor works expected', () => {
  const node: Node = new Node(1, 2, 3);
  expect(node.startColumnTotal).toBe(1);
  expect(node.startColumnRelative).toBe(2);
  expect(node.startLine).toBe(3);
});

test('Node.setError working correctly', () => {
  const node: Node = new Node(1, 2, 3);
  expect(node.hasError).toBeFalsy();
  node.setHasError(false);
  expect(node.hasError).toBeFalsy();
  node.setHasError(true);
  expect(node.hasError).toBeTruthy();
  node.setHasError(false);
  expect(node.hasError).toBeTruthy();
});

test('ProgramId constructor working correctly', () => {
  const node: Nodes.ProgramId = new Nodes.ProgramId(1, 2, 3, 'PGMNAME');
  expect(node.startColumnTotal).toBe(1);
  expect(node.startColumnRelative).toBe(2);
  expect(node.startLine).toBe(3);
  expect(node.programIdValue).toBe('PGMNAME');
});

test('IdentificationDivisionContent constructor working correctly', () => {
  const node: Nodes.IdentificationDivisionContent = new Nodes.IdentificationDivisionContent(
    1,
    2,
    3,
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
