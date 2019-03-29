/**
 * @fileoverview An AST Node
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

// import { Syntax } from './syntax';

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

/* tslint:disable:max-classes-per-file */

export class Node {
  public hasError: boolean = false;
  public type: string = '';
  public startColumnTotal: number = 0;
  public startColumnRelative: number = 0;
  public startLine: number = 0;
  public endColumnTotal: number = 0;
  public endColumnRelative: number = 0;
  public endLine: number = 0;

  constructor(startColumnTotal: number, startColumnRelative: number, startLine: number) {
    this.startColumnTotal = startColumnTotal;
    this.startColumnRelative = startColumnRelative;
    this.startLine = startLine;
  }

  /* istanbul ignore next */
  public toString = (): string => {
    return `Node (type: ${this.type},
            startColumnTotal: ${this.startColumnTotal},
            endColumnTotal: ${this.endColumnTotal},
            startColumnRelative: ${this.startColumnRelative},
            endColumnRelative: ${this.endColumnRelative},
            startLine: ${this.startLine},
            endLine: ${this.endLine}),
            hasError: ${this.hasError}
            \r\n`;
  };

  public setHasError(hasError: boolean) {
    // if once set to true it will stay to true
    this.hasError = this.hasError ? true : hasError;
  }
}

export class CobolSourceProgram extends Node {
  readonly programId: ProgramId;
  readonly identificationDivisionContent: IdentificationDivisionContent;
  readonly environmentDivisionContent: EnvironmentDivisionContent;

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    programId: ProgramId,
    identificationDivisionContent: IdentificationDivisionContent,
    environmentDivisionContent: EnvironmentDivisionContent,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.programId = programId;
    this.identificationDivisionContent = identificationDivisionContent;
    this.environmentDivisionContent = environmentDivisionContent;
  }
}
export class ProgramId extends Node {
  readonly programIdValue: string;
  constructor(startColumnTotal: number, startColumnRelative: number, startLine: number, programIdValue: string) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.programIdValue = programIdValue;
  }
}

export class IdentificationDivisionContent extends Node {
  readonly author: string;
  readonly installation: string;
  readonly dateWritten: string;
  readonly dateCompiled: string;
  readonly security: string;
  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    author: string,
    installation: string,
    dateWritten: string,
    dateCompiled: string,
    security: string,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.author = author;
    this.installation = installation;
    this.dateWritten = dateWritten;
    this.dateCompiled = dateCompiled;
    this.security = security;
  }
}
export class EnvironmentDivisionContent extends Node {
  readonly configurationSection: ConfigurationSection | null;
  readonly inputOutputSection: InputOutputSection | null;
  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    configurationSection: ConfigurationSection | null,
    inputOutputSection: InputOutputSection | null,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.configurationSection = configurationSection;
    this.inputOutputSection = inputOutputSection;
  }
}

export class ConfigurationSection extends Node {
  readonly sourceComputerParagraph: SourceComputerParagraph | null;
  readonly objectComputerParagraph: ObjectComputerParagraph | null;
  readonly specialNamesParagraph: SpecialNamesParagraph | null;
  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    sourceComputerParagraph: SourceComputerParagraph | null,
    objectComputerParagraph: ObjectComputerParagraph | null,
    specialNamesParagraph: SpecialNamesParagraph | null,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.sourceComputerParagraph = sourceComputerParagraph;
    this.objectComputerParagraph = objectComputerParagraph;
    this.specialNamesParagraph = specialNamesParagraph;
  }
}

export class SourceComputerParagraph extends Node {
  readonly sourceComputerValue: string;
  constructor(startColumnTotal: number, startColumnRelative: number, startLine: number, sourceComputerValue: string) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.sourceComputerValue = sourceComputerValue;
  }
}

export class ObjectComputerParagraph extends Node {
  readonly objectComputerValue: string;
  readonly memorySizeValue: string;
  readonly sequenceValue: string;
  readonly segmentLimitValue: string;

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    objectComputerValue: string,
    memorySizeValue: string,
    sequenceValue: string,
    segmentLimitValue: string,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.objectComputerValue = objectComputerValue;
    this.memorySizeValue = memorySizeValue;
    this.sequenceValue = sequenceValue;
    this.segmentLimitValue = segmentLimitValue;
  }
}
export class SpecialNamesParagraph extends Node {
  readonly specialNamesParagraphStatusPhrases: SpecialNamesParagraphStatusPhrase[];
  readonly specialNamesParagraphClauses: SpecialNamesParagraphClause[];
  readonly currencySignClause: CurrencySignClause | null = null;
  readonly decimalPointClause: DecimalPointClause | null = null;

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    specialNamesParagraphStatusPhrases: SpecialNamesParagraphStatusPhrase[],
    specialNamesParagraphClauses: SpecialNamesParagraphClause[],
    currencySignClause: CurrencySignClause | null,
    decimalPointClause: DecimalPointClause | null,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.specialNamesParagraphStatusPhrases = specialNamesParagraphStatusPhrases;
    this.specialNamesParagraphClauses = specialNamesParagraphClauses;
    this.currencySignClause = currencySignClause;
    this.decimalPointClause = decimalPointClause;
  }
}

export class SpecialNamesParagraphStatusPhrase extends Node {
  readonly environmentValue: string;
  readonly mnemonicValue: string;
  readonly onCondition: Condition | null;
  readonly offCondition: Condition | null;

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    environmentValue: string,
    mnemonicValue: string,
    onCondition: Condition | null,
    offCondition: Condition | null,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.environmentValue = environmentValue;
    this.mnemonicValue = mnemonicValue;
    this.onCondition = onCondition;
    this.offCondition = offCondition;
  }
}

export class InputOutputSection extends Node {}

export class RecordingModeClause extends Node {}

// TODO
export type Condition = CombinableCondition | CombinedCondition;
export class CombinableCondition extends Node {}
export class CombinedCondition extends Node {}

// TODO
export type SpecialNamesParagraphClause =
  | AlphabetClause
  | SymbolicCharactersClause
  | ClassClause
  | CurrencySignClause
  | DecimalPointClause;
export class AlphabetClause extends Node {
  readonly alphabetName: string = '';
  readonly alphabetType: string = '';
  readonly alphabetLiterals: Literal[] = [];

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    alphabetName: string,
    alphabetType: string,
    alphabetLiterals: Literal[],
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.alphabetName = alphabetName;
    this.alphabetType = alphabetType;
    this.alphabetLiterals = alphabetLiterals;
  }
}
export class SymbolicCharactersClause extends Node {
  readonly symbolicCharacters: string[] = [];
  readonly symbolicIntegers: string[] = [];
  readonly inOrdinalPosition: string = '';

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    symbolicCharacters: string[],
    symbolicIntegers: string[],
    inOrdinalPosition: string,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.symbolicCharacters = symbolicCharacters;
    this.symbolicIntegers = symbolicIntegers;
    this.inOrdinalPosition = inOrdinalPosition;
  }
}
export class ClassClause extends Node {
  readonly className: string = '';
  readonly classLiterals: Literal[] = [];

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    className: string,
    classLiterals: Literal[],
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.className = className;
    this.classLiterals = classLiterals;
  }
}
export class CurrencySignClause extends Node {
  readonly currencySignValue: string;

  constructor(startColumnTotal: number, startColumnRelative: number, startLine: number, currencySignValue: string) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.currencySignValue = currencySignValue;
  }
}
export class DecimalPointClause extends Node {
  // if Node is available isComma is always true
  readonly isComma: boolean = true;
}

export class Literal {
  literal: string = '';
  throughLiteral: string = '';
  alsoLiteral: string = '';
}

// export type ConfigurationSectionParagraph = SourceComputerParagraph | ObjectComputerParagraph | SpecialNamesParagraph;
// export type FileAndSortDescriptionEntryClauses = ExternalClause | GlobalClause | BlockContainsClause | RecordClause | LabelRecordsClause | ValueOfClause | DataRecordsClause | LinageClause | RecordingModeClause | CodeSetClause;
// export type DataDescriptionEntryClauses = RedefinesClause | BlankWhenZeroClause | JustifiedClause | OccursClause | PictureClause | SignClause | SyncronizedClause | UsageClause | ConditionValueClause | DataValueClause | RenamesClause;
// export type Sections = Paragraphs;
// export type Paragraphs = Sentence | Paragraph;
// export type Statement = AcceptStatement | AddStatement | AlterStatement | CallStatement | CancelStatement | CloseStatement | ComputeStatement | ContinueStatement | DeleteStatement | DisplayStatement | DivideStatement | EntryStatement
//     | EvaluateStatement | ExitStatement | ExitProgramStatement | GoBackStatement | GoToStatement | AlteredGoToStatement | IfStatement | InitializeStatement | InspectStatement | MergeStatement | MoveStatement | MultiplyStatement | OpenStatement
//     | PerformStatement | PerformUntilStatement | PerformVaryingStatement | PerformAfterStatement | ReadStatement | ReleaseStatement | ReturnStatement | RewriteStatement | SearchStatement | SetStatement | SortStatement | StartStatement | StopStatement
//     | StringStatement | SubtractStatement | UnstringStatement | WriteStatement;
// export type CompilerDirectingStatement = BasisStatement | CblProcessStatement | ControlCblStatement | CopyStatement | DeleteCompilerDirectingStatement | EjectStatement | EnterStatement | InsertStatement | ReadyOrResetTraceStatement | ReplaceStatement
//     | ServiceLabelStatement | ServiceReloadStatement | SkipStatement | TitleStatement | UseStatement;
// export type SimpleCondition = ClassCondition | ConditionNameCondition | RelationCondition | SignCondition | SwitchStatusCondition | NegatedSimpleCondition;
// export type Name = AlphabetName | ClassName | ConditionName | FileName | IndexName | MnemonicName | RecordName | RoutineName | SymbolicCharacter | LibraryName | ProgramName | TextName | ParagraphName | SectionName | ComputerName | LanguageName | EnvironmentName
// | AssignmentName | BasisName;
