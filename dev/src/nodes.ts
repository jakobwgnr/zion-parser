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
  readonly configurationSection: ConfigurationSection | undefined;
  readonly inputOutputSection: InputOutputSection | undefined;
  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    configurationSection: ConfigurationSection | undefined,
    inputOutputSection: InputOutputSection | undefined,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.configurationSection = configurationSection;
    this.inputOutputSection = inputOutputSection;
  }
}

export class ConfigurationSection extends Node {
  readonly sourceComputerParagraph: SourceComputerParagraph | undefined;
  readonly objectComputerParagraph: ObjectComputerParagraph | undefined;
  readonly specialNamesParagraph: SpecialNamesParagraph | undefined;
  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    sourceComputerParagraph: SourceComputerParagraph | undefined,
    objectComputerParagraph: ObjectComputerParagraph | undefined,
    specialNamesParagraph: SpecialNamesParagraph | undefined,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.sourceComputerParagraph = sourceComputerParagraph;
    this.objectComputerParagraph = objectComputerParagraph;
    this.specialNamesParagraph = specialNamesParagraph;
  }
}

export class SourceComputerParagraph extends Node {
  readonly sourceComputerValue: string | undefined;
  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    sourceComputerValue: string | undefined,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.sourceComputerValue = sourceComputerValue;
  }
}

export class ObjectComputerParagraph extends Node {
  readonly objectComputerValue: string | undefined;
  readonly memorySizeValue: string | undefined;
  readonly sequenceValue: string | undefined;
  readonly segmentLimitValue: string | undefined;

  constructor(
    startColumnTotal: number,
    startColumnRelative: number,
    startLine: number,
    objectComputerValue: string | undefined,
    memorySizeValue: string | undefined,
    sequenceValue: string | undefined,
    segmentLimitValue: string | undefined,
  ) {
    super(startColumnTotal, startColumnRelative, startLine);
    this.objectComputerValue = objectComputerValue;
    this.memorySizeValue = memorySizeValue;
    this.sequenceValue = sequenceValue;
    this.segmentLimitValue = segmentLimitValue;
  }
}
export class SpecialNamesParagraph extends Node {}

export class InputOutputSection extends Node {}

export class RecordingModeClause extends Node {}

// export type ConfigurationSectionParagraph = SourceComputerParagraph | ObjectComputerParagraph | SpecialNamesParagraph;
// export type SpecialNamesParagraphClause = AlphabetClause | SymbolicCharactersClause | ClassClause | CurrencySignClause | DecimalPointClause;
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
// export type Condition = CombinableCondition | CombinedCondition;
// export type SimpleCondition = ClassCondition | ConditionNameCondition | RelationCondition | SignCondition | SwitchStatusCondition | NegatedSimpleCondition;
// export type Name = AlphabetName | ClassName | ConditionName | FileName | IndexName | MnemonicName | RecordName | RoutineName | SymbolicCharacter | LibraryName | ProgramName | TextName | ParagraphName | SectionName | ComputerName | LanguageName | EnvironmentName
// | AssignmentName | BasisName;
