import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Single Digit Level to be parsed correctly', () => {
  const ast: Ast = zionParser.parse('         1 DATA-NAME REDEFINES REDFINE-NAME.', {
    fromPath: false,
  });
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('1');
});

test('2-Digit Level to be parsed correctly', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME REDEFINES REDFINE-NAME.', {
    fromPath: false,
  });
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
});

test('2-Digit Level out of range to throw error', () => {
  const ast: Ast = zionParser.parse('         55 DATA-NAME REDEFINES REDFINE-NAME.', {
    fromPath: false,
  });
  expect(ast.errors.length).toBeGreaterThan(0);
});

test('Data Description entry with dataName + Redefines Clause', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME REDEFINES REDFINE-NAME.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.RedefinesClause);
  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RedefinesClause).dataName,
  ).toBe('REDFINE-NAME');
});

test('Data Description entry with FILLER + Redefines Clause', () => {
  const ast: Ast = zionParser.parse('         01 FILLER REDEFINES REDFINE-NAME.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('FILLER');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.RedefinesClause);
  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RedefinesClause).dataName,
  ).toBe('REDFINE-NAME');
});

// Renames Clause - Level 66

test('Renames Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse(
    '         66 DATA-NAME RENAMES DATA-NAME2\r\n' +
      '                           IN DATA-NAME3\r\n' +
      '                           OF DATA-NAME4\r\n' +
      '                      THROUGH DATA-NAME5\r\n' +
      '                           IN DATA-NAME6\r\n' +
      '                           OF DATA-NAME7.\r\n',
    {
      fromPath: false,
    },
  );
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('66');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.RenamesClause);
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RenamesClause)
      .renamesQualifiedDataName as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME2');
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RenamesClause)
      .renamesQualifiedDataName as Nodes.QualifiedDataName).dataNames[0],
  ).toBe('DATA-NAME3');
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RenamesClause)
      .renamesQualifiedDataName as Nodes.QualifiedDataName).dataNames[1],
  ).toBe('DATA-NAME4');
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RenamesClause)
      .throughQualifiedDataName as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME5');
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RenamesClause)
      .throughQualifiedDataName as Nodes.QualifiedDataName).dataNames[0],
  ).toBe('DATA-NAME6');
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RenamesClause)
      .throughQualifiedDataName as Nodes.QualifiedDataName).dataNames[1],
  ).toBe('DATA-NAME7');
});

test('Renames Data Description entry with special register - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         66 DATA-NAME RENAMES DEBUG-ITEM.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('66');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.RenamesClause);
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.RenamesClause)
      .renamesQualifiedDataName as Nodes.QualifiedDataName).value as Nodes.SpecialRegister).specialRegisterType,
  ).toBe('DEBUG-ITEM');
});

test('BlankWhenZero Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME BLANK WHEN ZEROES.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(
    Syntax.BlankWhenZeroClause,
  );
});

test('IsExternal Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME IS EXTERNAL.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.ExternalClause);
});

test('IsGlobal Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME IS GLOBAL.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.GlobalClause);
});

test('Justified Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME JUSTIFIED RIGHT.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.JustifiedClause);
});

test('SyncronizedClause Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME SYNCHRONIZED RIGHT.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(
    Syntax.SyncronizedClause,
  );

  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.SyncronizedClause).value,
  ).toBe('RIGHT');
});

test('UsageClause Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME USAGE IS BINARY.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.UsageClause);

  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.UsageClause).value).toBe(
    'BINARY',
  );
});

test('DataValueClause Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME VALUE IS "VALUE".', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.DataValueClause);

  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.DataValueClause)
      .value as Nodes.Literal).value,
  ).toBe('"VALUE"');
});

test('SignClause Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME SIGN IS LEADING SEPARATE CHARACTER.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.SignClause);

  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.SignClause).value).toBe(
    'LEADING',
  );
});

test('Occurs Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse(
    '         01 DATA-NAME OCCURS 10 TIMES\r\n' +
      '                      ASCENDING KEY IS DATA-NAME-A1 DATA-NAME-A2\r\n' +
      '                      DESCENDING KEY IS DATA-NAME-D1 DATA-NAME-D2\r\n' +
      '                      INDEXED BY INDEX-NAME INDEX-NAME2.',
    {
      fromPath: false,
    },
  );
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.OccursClause);
  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause).occursValue,
  ).toBe('10');

  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[0] as Nodes.OccursKey).orderType,
  ).toBe('ASCENDING');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[0] as Nodes.OccursKey).keys[0] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-A1');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[0] as Nodes.OccursKey).keys[1] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-A2');

  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[1] as Nodes.OccursKey).orderType,
  ).toBe('DESCENDING');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[1] as Nodes.OccursKey).keys[0] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-D1');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[1] as Nodes.OccursKey).keys[1] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-D2');

  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause).indexNames[0],
  ).toBe('INDEX-NAME');

  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause).indexNames[1],
  ).toBe('INDEX-NAME2');
});

test('Occurs From To Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse(
    '         01 DATA-NAME OCCURS 10 TO 20 TIMES\r\n' +
      '                      DEPENDING ON DATA-NAME-DEP\r\n' +
      '                      ASCENDING KEY IS DATA-NAME-A1 DATA-NAME-A2\r\n' +
      '                      DESCENDING KEY IS DATA-NAME-D1 DATA-NAME-D2\r\n' +
      '                      INDEXED BY INDEX-NAME INDEX-NAME2.',
    {
      fromPath: false,
    },
  );
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.OccursClause);

  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause).occursValue,
  ).toBe('10');
  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause).occursToValue,
  ).toBe('20');

  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .dependingOnQualifiedDataName as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-DEP');

  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[0] as Nodes.OccursKey).orderType,
  ).toBe('ASCENDING');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[0] as Nodes.OccursKey).keys[0] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-A1');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[0] as Nodes.OccursKey).keys[1] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-A2');

  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[1] as Nodes.OccursKey).orderType,
  ).toBe('DESCENDING');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[1] as Nodes.OccursKey).keys[0] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-D1');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause)
      .occursKeys[1] as Nodes.OccursKey).keys[1] as Nodes.QualifiedDataName).value,
  ).toBe('DATA-NAME-D2');

  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause).indexNames[0],
  ).toBe('INDEX-NAME');

  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.OccursClause).indexNames[1],
  ).toBe('INDEX-NAME2');
});

test('Condition Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse(
    '         88 CONDITION-NAME VALUES ARE ZERO THROUGH 1000\r\n' +
      '                                    "LITERAL3" THROUGH "LITERAL4"' +
      '                                    ZEROS THROUGH ZEROES' +
      '                                    SPACE THROUGH SPACES' +
      '                                    HIGH-VALUE THROUGH HIGH-VALUES' +
      '                                    LOW-VALUE THROUGH LOW-VALUES' +
      '                                    QUOTE THROUGH QUOTES' +
      '                                    ALL "OPT-LITERAL" THROUGH QUOTE' +
      '                                    NULL THROUGH NULLS' +
      '                                    "LITERAL3" THROUGH "LITERAL4".',
    {
      fromPath: false,
    },
  );
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('88');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('CONDITION-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(
    Syntax.ConditionValueClause,
  );

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[0] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('ZERO');
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[0] as Nodes.Literal).value,
  ).toBe('1000');

  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[1] as Nodes.Literal).value,
  ).toBe('"LITERAL3"');
  expect(
    (((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[1] as Nodes.Literal).value,
  ).toBe('"LITERAL4"');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[2] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('ZEROS');
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[2] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('ZEROES');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[3] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('SPACE');
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[3] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('SPACES');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[4] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('HIGH-VALUE');
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[4] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('HIGH-VALUES');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[5] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('LOW-VALUE');
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[5] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('LOW-VALUES');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[6] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('QUOTE');
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[6] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('QUOTES');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[7] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('ALL');
  expect(
    (((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[7] as Nodes.Literal).value as Nodes.FigurativeConstant).optionalValue as Nodes.Literal).value,
  ).toBe('"OPT-LITERAL"');
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[7] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('QUOTE');

  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .valueLiterals[8] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('NULL');
  expect(
    ((((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.ConditionValueClause)
      .throughLiterals[8] as Nodes.Literal).value as Nodes.FigurativeConstant).figurativeConstantType,
  ).toBe('NULLS');
});

test('Two Data Description entries identified correctly', () => {
  const ast: Ast = zionParser.parse(
    '         01 FILLER REDEFINES REDFINE-NAME.\r\n' + '         01 FILLER REDEFINES REDFINE-NAME.\r\n',
    {
      fromPath: false,
    },
  );
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.nodes[1].type).toBe(Syntax.DataDescriptionEntry);
});

test('Missing Terminator throws error', () => {
  const ast: Ast = zionParser.parse('         01 FILLER REDEFINES REDFINE-NAME\r\n', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBeGreaterThan(0);
});

test('Data Description entry Redefines Clause, but wrong level - error expected', () => {
  const ast: Ast = zionParser.parse('         88 FILLER REDEFINES REDFINE-NAME.', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBeGreaterThan(0);
});
