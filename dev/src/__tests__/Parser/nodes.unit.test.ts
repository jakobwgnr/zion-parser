import { Node } from '../../Parser/nodes';

test('Node Constructor works expected', () => {
  const node: Node = new Node(1, 2, 3);
  expect(node.startColumnTotal).toBe(1);
  expect(node.startColumnRelative).toBe(2);
  expect(node.startLine).toBe(3);
});
