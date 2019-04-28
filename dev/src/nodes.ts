/**
 * @fileoverview An AST Node
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

import { Syntax } from './syntax';

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

/* tslint:disable:max-classes-per-file */

export interface NodeStandardInfo {
  startColumnTotal: number;
  startColumnRelative: number;
  startLine: number;
}
export abstract class Node {
  public hasError: boolean = false;
  public type: string = '';
  public startColumnTotal: number = 0;
  public startColumnRelative: number = 0;
  public startLine: number = 0;
  public endColumnTotal: number = 0;
  public endColumnRelative: number = 0;
  public endLine: number = 0;

  constructor(info: NodeStandardInfo) {
    this.startColumnTotal = info.startColumnTotal;
    this.startColumnRelative = info.startColumnRelative;
    this.startLine = info.startLine;
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
}

export class CobolSourceProgram extends Node {
  readonly programId: ProgramId;
  readonly identificationDivisionContent: IdentificationDivisionContent;
  readonly environmentDivisionContent: EnvironmentDivisionContent;

  constructor(
    info: NodeStandardInfo,
    programId: ProgramId,
    identificationDivisionContent: IdentificationDivisionContent,
    environmentDivisionContent: EnvironmentDivisionContent,
  ) {
    super(info);
    this.programId = programId;
    this.identificationDivisionContent = identificationDivisionContent;
    this.environmentDivisionContent = environmentDivisionContent;
    this.type = Syntax.CobolSourceProgram;
  }
}
export class ProgramId extends Node {
  readonly programIdValue: string;
  constructor(info: NodeStandardInfo, programIdValue: string) {
    super(info);
    this.programIdValue = programIdValue;
    this.type = Syntax.ProgramId;
  }
}

export class IdentificationDivisionContent extends Node {
  readonly author: string;
  readonly installation: string;
  readonly dateWritten: string;
  readonly dateCompiled: string;
  readonly security: string;
  constructor(
    info: NodeStandardInfo,
    author: string,
    installation: string,
    dateWritten: string,
    dateCompiled: string,
    security: string,
  ) {
    super(info);
    this.author = author;
    this.installation = installation;
    this.dateWritten = dateWritten;
    this.dateCompiled = dateCompiled;
    this.security = security;
    this.type = Syntax.IdentificationDivisionContent;
  }
}
export class EnvironmentDivisionContent extends Node {
  readonly configurationSection: ConfigurationSection | null;
  readonly inputOutputSection: InputOutputSection | null;
  constructor(
    info: NodeStandardInfo,
    configurationSection: ConfigurationSection | null,
    inputOutputSection: InputOutputSection | null,
  ) {
    super(info);
    this.configurationSection = configurationSection;
    this.inputOutputSection = inputOutputSection;
    this.type = Syntax.EnvironmentDivisionContent;
  }
}

export class ConfigurationSection extends Node {
  readonly sourceComputerParagraph: SourceComputerParagraph | null;
  readonly objectComputerParagraph: ObjectComputerParagraph | null;
  readonly specialNamesParagraph: SpecialNamesParagraph | null;
  constructor(
    info: NodeStandardInfo,
    sourceComputerParagraph: SourceComputerParagraph | null,
    objectComputerParagraph: ObjectComputerParagraph | null,
    specialNamesParagraph: SpecialNamesParagraph | null,
  ) {
    super(info);
    this.sourceComputerParagraph = sourceComputerParagraph;
    this.objectComputerParagraph = objectComputerParagraph;
    this.specialNamesParagraph = specialNamesParagraph;
    this.type = Syntax.ConfigurationSection;
  }
}

export class SourceComputerParagraph extends Node {
  readonly sourceComputerValue: string;
  constructor(info: NodeStandardInfo, sourceComputerValue: string) {
    super(info);
    this.sourceComputerValue = sourceComputerValue;
    this.type = Syntax.SourceComputerParagraph;
  }
}

export class ObjectComputerParagraph extends Node {
  readonly objectComputerValue: string;
  readonly memorySizeValue: string;
  readonly sequenceValue: string;
  readonly segmentLimitValue: string;

  constructor(
    info: NodeStandardInfo,
    objectComputerValue: string,
    memorySizeValue: string,
    sequenceValue: string,
    segmentLimitValue: string,
  ) {
    super(info);
    this.objectComputerValue = objectComputerValue;
    this.memorySizeValue = memorySizeValue;
    this.sequenceValue = sequenceValue;
    this.segmentLimitValue = segmentLimitValue;
    this.type = Syntax.ObjectComputerParagraph;
  }
}
export class SpecialNamesParagraph extends Node {
  readonly specialNamesParagraphStatusPhrases: SpecialNamesParagraphStatusPhrase[];
  readonly specialNamesParagraphClauses: SpecialNamesParagraphClause[];
  readonly currencySignClause: CurrencySignClause | null = null;
  readonly decimalPointClause: DecimalPointClause | null = null;

  constructor(
    info: NodeStandardInfo,
    specialNamesParagraphStatusPhrases: SpecialNamesParagraphStatusPhrase[],
    specialNamesParagraphClauses: SpecialNamesParagraphClause[],
    currencySignClause: CurrencySignClause | null,
    decimalPointClause: DecimalPointClause | null,
  ) {
    super(info);
    this.specialNamesParagraphStatusPhrases = specialNamesParagraphStatusPhrases;
    this.specialNamesParagraphClauses = specialNamesParagraphClauses;
    this.currencySignClause = currencySignClause;
    this.decimalPointClause = decimalPointClause;
    this.type = Syntax.SpecialNamesParagraph;
  }
}

export class SpecialNamesParagraphStatusPhrase extends Node {
  readonly environmentValue: string;
  readonly mnemonicValue: string;
  readonly onCondition: Condition | null;
  readonly offCondition: Condition | null;

  constructor(
    info: NodeStandardInfo,
    environmentValue: string,
    mnemonicValue: string,
    onCondition: Condition | null,
    offCondition: Condition | null,
  ) {
    super(info);
    this.environmentValue = environmentValue;
    this.mnemonicValue = mnemonicValue;
    this.onCondition = onCondition;
    this.offCondition = offCondition;
    this.type = Syntax.SpecialNamesParagraphStatusPhrase;
  }
}

export class InputOutputSection extends Node {
  readonly fileControlParagraph: FileControlParagraph | null;
  readonly ioControlParagraph: IOControlParagraph | null;

  constructor(
    info: NodeStandardInfo,
    fileControlParagraph: FileControlParagraph | null,
    ioControlParagraph: IOControlParagraph | null,
  ) {
    super(info);
    this.type = Syntax.InputOutputSection;
    this.fileControlParagraph = fileControlParagraph;
    this.ioControlParagraph = ioControlParagraph;
  }
}

export class FileControlParagraph extends Node {
  readonly fileControlEntries: FileControlEntry[];
  constructor(info: NodeStandardInfo, fileControlEntries: FileControlEntry[]) {
    super(info);
    this.type = Syntax.FileControlParagraph;
    this.fileControlEntries = fileControlEntries;
  }
}

export class FileControlEntry extends Node {
  readonly selectClause: SelectClause | null;
  readonly assignClause: AssignClause | null;

  constructor(info: NodeStandardInfo, selectClause: SelectClause | null, assignClause: AssignClause | null) {
    super(info);
    this.type = Syntax.FileControlEntry;
    this.selectClause = selectClause;
    this.assignClause = assignClause;
  }
}

export class SelectClause extends Node {
  fileName: string = '';
  constructor(info: NodeStandardInfo, fileName: string) {
    super(info);
    this.type = Syntax.SelectClause;
    this.fileName = fileName;
  }
}

export class AssignClause extends Node {
  assignmentName: string[] = [];
  constructor(info: NodeStandardInfo, assignmentName: string[]) {
    super(info);
    this.type = Syntax.AssignClause;
    this.assignmentName = assignmentName;
  }
}

export class ReserveClause extends Node {
  reserveAreaCount: string = '';
  constructor(info: NodeStandardInfo, reserveAreaCount: string) {
    super(info);
    this.type = Syntax.ReserveClause;
    this.reserveAreaCount = reserveAreaCount;
  }
}

export class PaddingCharacterClause extends Node {
  qualifiedDataName: QualifiedDataName;
  constructor(info: NodeStandardInfo, qualifiedDataName: QualifiedDataName) {
    super(info);
    this.type = Syntax.PaddingCharacterClause;
    this.qualifiedDataName = qualifiedDataName;
  }
}

export class RecordDelimiterClause extends Node {
  delimiterValue: string = '';
  constructor(info: NodeStandardInfo, delimiterValue: string) {
    super(info);
    this.type = Syntax.RecordDelimiterClause;
    this.delimiterValue = delimiterValue;
  }
}

export class RecordKeyClause extends Node {
  delimiterValue: string = '';
  dataNames: string[] = [];
  constructor(info: NodeStandardInfo, delimiterValue: string, dataNames: string[]) {
    super(info);
    this.type = Syntax.RecordKeyClause;
    this.delimiterValue = delimiterValue;
    this.dataNames = dataNames;
  }
}

export class AlternateRecordKeyClause extends Node {
  value: string = '';
  dataNames: string[] = [];
  passwordClause: PasswordClause | null = null;
  constructor(info: NodeStandardInfo, value: string, dataNames: string[], passwordClause: PasswordClause | null) {
    super(info);
    this.type = Syntax.AlternateRecordKeyClause;
    this.value = value;
    this.dataNames = dataNames;
    this.passwordClause = passwordClause;
  }
}

export class RelativeKeyClause extends Node {
  value: string = '';
  dataNames: string[] = [];
  constructor(info: NodeStandardInfo, value: string, dataNames: string[]) {
    super(info);
    this.type = Syntax.RelativeKeyClause;
    this.value = value;
    this.dataNames = dataNames;
  }
}

export class FileStatusClause extends Node {
  value: string = '';
  optionalValue: string = '';
  dataNames: string[] = [];
  optionalDataNames: string[] = [];
  constructor(
    info: NodeStandardInfo,
    value: string,
    dataNames: string[],
    optionalValue: string,
    optionalDataNames: string[],
  ) {
    super(info);
    this.type = Syntax.FileStatusClause;
    this.value = value;
    this.dataNames = dataNames;
    this.optionalValue = optionalValue;
    this.optionalDataNames = optionalDataNames;
  }
}

export class PasswordClause extends Node {
  value: string = '';
  constructor(info: NodeStandardInfo, value: string) {
    super(info);
    this.type = Syntax.PasswordClause;
    this.value = value;
  }
}

export class SpecialRegister extends Node {
  specialRegisterType: string = '';
  optionalValue: string = '';
  constructor(info: NodeStandardInfo, specialRegisterType: string, optionalValue: string) {
    super(info);
    this.type = Syntax.SpecialRegister;
    this.specialRegisterType = specialRegisterType;
    this.optionalValue = optionalValue;
  }
}

export class FigurativeConstant extends Node {
  figurativeConstantType: string = '';
  optionalValue: Literal | null;
  constructor(info: NodeStandardInfo, figurativeConstantType: string, optionalValue: Literal | null) {
    super(info);
    this.type = Syntax.FigurativeConstant;
    this.figurativeConstantType = figurativeConstantType;
    this.optionalValue = optionalValue;
  }
}

export class QuotedPseudoText extends Node {
  value: string = '';
  constructor(info: NodeStandardInfo, value: string) {
    super(info);
    this.type = Syntax.QuotedPseudoText;
    this.value = value;
  }
}

export class IOControlParagraph extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.IOControlParagraph;
  }
}

export class RecordingModeClause extends Node {
  readonly recordingMode: string;
  constructor(info: NodeStandardInfo, recordingMode: string) {
    super(info);
    this.recordingMode = recordingMode;
    this.type = Syntax.RecordingModeClause;
  }
}

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
  readonly alphabetLiterals: AlphabetLiteral[] = [];

  constructor(info: NodeStandardInfo, alphabetName: string, alphabetType: string, alphabetLiterals: AlphabetLiteral[]) {
    super(info);
    this.alphabetName = alphabetName;
    this.alphabetType = alphabetType;
    this.alphabetLiterals = alphabetLiterals;
    this.type = Syntax.AlphabetClause;
  }
}
export class SymbolicCharactersClause extends Node {
  readonly symbolicCharacters: string[] = [];
  readonly symbolicIntegers: string[] = [];
  readonly inOrdinalPosition: string = '';

  constructor(
    info: NodeStandardInfo,
    symbolicCharacters: string[],
    symbolicIntegers: string[],
    inOrdinalPosition: string,
  ) {
    super(info);
    this.symbolicCharacters = symbolicCharacters;
    this.symbolicIntegers = symbolicIntegers;
    this.inOrdinalPosition = inOrdinalPosition;
    this.type = Syntax.SymbolicCharactersClause;
  }
}
export class ClassClause extends Node {
  readonly className: string = '';
  readonly classLiterals: AlphabetLiteral[] = [];

  constructor(info: NodeStandardInfo, className: string, classLiterals: AlphabetLiteral[]) {
    super(info);
    this.className = className;
    this.classLiterals = classLiterals;
    this.type = Syntax.ClassClause;
  }
}
export class CurrencySignClause extends Node {
  readonly currencySignValue: string;

  constructor(info: NodeStandardInfo, currencySignValue: string) {
    super(info);
    this.currencySignValue = currencySignValue;
    this.type = Syntax.CurrencySignClause;
  }
}
export class DecimalPointClause extends Node {
  // if Node is available isComma is always true
  readonly isComma: boolean = true;
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.DecimalPointClause;
  }
}

export class Literal extends Node {
  readonly value: number | string | FigurativeConstant;
  constructor(info: NodeStandardInfo, value: number | string | FigurativeConstant) {
    super(info);
    this.type = Syntax.Literal;
    this.value = value;
  }
}

export class BasisStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.BasisStatement;
  }
}

export class CblProcessStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.CblProcessStatement;
  }
}

export class ControlCblStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.ControlCblStatement;
  }
}

export class CopyStatement extends Node {
  readonly value: string | Literal;
  readonly libraryName: string | Literal;
  readonly replacingCopyOperands: CopyOperand[];
  readonly byCopyOperands: CopyOperand[];
  constructor(
    info: NodeStandardInfo,
    value: string | Literal,
    libraryName: string | Literal,
    replacingCopyOperands: CopyOperand[],
    byCopyOperands: CopyOperand[],
  ) {
    super(info);
    this.type = Syntax.CopyStatement;
    this.value = value;
    this.libraryName = libraryName;
    this.replacingCopyOperands = replacingCopyOperands;
    this.byCopyOperands = byCopyOperands;
  }
}

export class CopyOperand extends Node {
  readonly value: string | QuotedPseudoText | Identifier | Literal;
  constructor(info: NodeStandardInfo, value: string | QuotedPseudoText | Identifier | Literal) {
    super(info);
    this.type = Syntax.CopyOperand;
    this.value = value;
  }
}

export class DeleteCompilerDirectingStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.DeleteCompilerDirectingStatement;
  }
}

export class EjectStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.ReadyOrResetTraceStatement;
  }
}

export class EnterStatement extends Node {
  readonly languageName: string;
  readonly routineName: string;
  constructor(info: NodeStandardInfo, languageName: string, routineName: string) {
    super(info);
    this.type = Syntax.ReadyOrResetTraceStatement;
    this.languageName = languageName;
    this.routineName = routineName;
  }
}

export class InsertStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.InsertStatement;
  }
}

export class ReadyOrResetTraceStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.ReadyOrResetTraceStatement;
  }
}

export class ReplaceStatement extends Node {
  readonly quotedPseudoText: QuotedPseudoText[];
  readonly byQuotedPseudoText: QuotedPseudoText[];
  constructor(info: NodeStandardInfo, quotedPseudoText: QuotedPseudoText[], byQuotedPseudoText: QuotedPseudoText[]) {
    super(info);
    this.type = Syntax.ReplaceStatement;
    this.quotedPseudoText = quotedPseudoText;
    this.byQuotedPseudoText = byQuotedPseudoText;
  }
}

export type ServiceStatement = ServiceLabelStatement | ServiceReloadStatement;
export class ServiceLabelStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.ServiceLabelStatement;
  }
}

export class ServiceReloadStatement extends Node {
  readonly identifier: Identifier;
  constructor(info: NodeStandardInfo, identifier: Identifier) {
    super(info);
    this.type = Syntax.ServiceReloadStatement;
    this.identifier = identifier;
  }
}

export class SkipStatement extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.SkipStatement;
  }
}

export class TitleStatement extends Node {
  readonly literal: Literal;
  constructor(info: NodeStandardInfo, literal: Literal) {
    super(info);
    this.type = Syntax.TitleStatement;
    this.literal = literal;
  }
}

export type Identifier = QualifiedIdentifier | LinageCounterIdentifier;
// TODO
export class QualifiedIdentifier extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
  }
}

export class LinageCounterIdentifier extends Node {
  filename: string;
  constructor(info: NodeStandardInfo, filename: string) {
    super(info);
    this.filename = filename;
    this.type = Syntax.Identifier;
  }
}

export class QualifiedDataName extends Node {
  value: string | SpecialRegister = '';
  dataNames: string[] = [];
  constructor(info: NodeStandardInfo, value: string | SpecialRegister, dataNames: string[]) {
    super(info);
    this.type = Syntax.QualifiedDataName;
    this.value = value;
    this.dataNames = dataNames;
  }
}

export class LevelNumber extends Node {
  value: string = '';
  dataNames: string[] = [];
  constructor(info: NodeStandardInfo, value: string) {
    super(info);
    this.type = Syntax.LevelNumber;
    this.value = value;
  }
}

export class DataDescriptionEntry extends Node {
  readonly level: LevelNumber;
  readonly dataName: string;
  readonly dataDescriptonEntryClauses: DataDescriptionEntryClause[] | RenamesClause[] | ConditionValueClause[];

  constructor(
    info: NodeStandardInfo,
    level: LevelNumber,
    dataName: string,
    dataDescriptonEntryClauses: DataDescriptionEntryClause[] | RenamesClause[] | ConditionValueClause[],
  ) {
    super(info);
    this.type = Syntax.DataDescriptionEntry;
    this.level = level;
    this.dataName = dataName;
    this.dataDescriptonEntryClauses = dataDescriptonEntryClauses;
  }
}

export type DataDescriptionEntryClause =
  | RedefinesClause
  | BlankWhenZeroClause
  | JustifiedClause
  | ExternalClause
  | GlobalClause
  | OccursClause
  | PictureClause
  | SignClause
  | SyncronizedClause
  | UsageClause
  | ConditionValueClause
  | DataValueClause
  | RenamesClause;

export class RedefinesClause extends Node {
  readonly dataName: string;

  constructor(info: NodeStandardInfo, dataName: string) {
    super(info);
    this.type = Syntax.RedefinesClause;
    this.dataName = dataName;
  }
}

export class BlankWhenZeroClause extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.BlankWhenZeroClause;
  }
}

export class GlobalClause extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.GlobalClause;
  }
}

export class ExternalClause extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.ExternalClause;
  }
}

export class JustifiedClause extends Node {
  constructor(info: NodeStandardInfo) {
    super(info);
    this.type = Syntax.JustifiedClause;
  }
}

export interface OccursKey {
  orderType: string;
  keys: QualifiedDataName[];
}

export class OccursClause extends Node {
  readonly occursValue: string;
  readonly occursToValue: string;
  readonly occursKeys: OccursKey[];
  readonly dependingOnQualifiedDataName: QualifiedDataName | null;
  readonly indexNames: string[];
  constructor(
    info: NodeStandardInfo,
    occursValue: string,
    occursToValue: string,
    dependingOnQualifiedDataName: QualifiedDataName | null,
    occursKeys: OccursKey[],
    indexNames: string[],
  ) {
    super(info);
    this.type = Syntax.OccursClause;
    this.occursValue = occursValue;
    this.occursToValue = occursToValue;
    this.dependingOnQualifiedDataName = dependingOnQualifiedDataName;
    this.occursKeys = occursKeys;
    this.indexNames = indexNames;
  }
}

export class PictureClause extends Node {
  readonly pictureString: PictureString;
  constructor(info: NodeStandardInfo, pictureString: PictureString) {
    super(info);
    this.type = Syntax.PictureClause;
    this.pictureString = pictureString;
  }
}

export interface PictureValue {
  value: string;
  length: number;
}
export class PictureString extends Node {
  readonly picValues: PictureValue[];

  constructor(info: NodeStandardInfo, picValues: PictureValue[]) {
    super(info);
    this.type = Syntax.PictureString;
    this.picValues = picValues;
  }
}

export class SignClause extends Node {
  readonly value: string;
  constructor(info: NodeStandardInfo, value: string) {
    super(info);
    this.type = Syntax.SignClause;
    this.value = value;
  }
}

export class SyncronizedClause extends Node {
  readonly value: string;
  constructor(info: NodeStandardInfo, value: string) {
    super(info);
    this.type = Syntax.SyncronizedClause;
    this.value = value;
  }
}

export class UsageClause extends Node {
  readonly value: string;
  constructor(info: NodeStandardInfo, value: string) {
    super(info);
    this.type = Syntax.UsageClause;
    this.value = value;
  }
}

export class DataValueClause extends Node {
  readonly value: Literal;

  constructor(info: NodeStandardInfo, value: Literal) {
    super(info);
    this.type = Syntax.DataValueClause;
    this.value = value;
  }
}

export class RenamesClause extends Node {
  readonly renamesQualifiedDataName: QualifiedDataName;
  readonly throughQualifiedDataName: QualifiedDataName | null;

  constructor(
    info: NodeStandardInfo,
    renamesQualifiedDataName: QualifiedDataName,
    throughQualifiedDataName: QualifiedDataName | null,
  ) {
    super(info);
    this.type = Syntax.RenamesClause;
    this.renamesQualifiedDataName = renamesQualifiedDataName;
    this.throughQualifiedDataName = throughQualifiedDataName;
  }
}

export class ConditionValueClause extends Node {
  readonly valueLiterals: Literal[];
  readonly throughLiterals: Literal[];

  constructor(info: NodeStandardInfo, valueLiterals: Literal[], throughLiterals: Literal[]) {
    super(info);
    this.type = Syntax.ConditionValueClause;
    this.valueLiterals = valueLiterals;
    this.throughLiterals = throughLiterals;
  }
}

export class AlphabetLiteral {
  literal: string = '';
  throughLiteral: string = '';
  alsoLiteral: string = '';
}

// export type ConfigurationSectionParagraph = SourceComputerParagraph | ObjectComputerParagraph | SpecialNamesParagraph;
// export type FileAndSortDescriptionEntryClauses = ExternalClause | GlobalClause | BlockContainsClause | RecordClause | LabelRecordsClause | ValueOfClause | DataRecordsClause | LinageClause | RecordingModeClause | CodeSetClause;
// export type Sections = Paragraphs;
// export type Paragraphs = Sentence | Paragraph;
// export type Statement = AcceptStatement | AddStatement | AlterStatement | CallStatement | CancelStatement | CloseStatement | ComputeStatement | ContinueStatement | DeleteStatement | DisplayStatement | DivideStatement | EntryStatement
//     | EvaluateStatement | ExitStatement | ExitProgramStatement | GoBackStatement | GoToStatement | AlteredGoToStatement | IfStatement | InitializeStatement | InspectStatement | MergeStatement | MoveStatement | MultiplyStatement | OpenStatement
//     | PerformStatement | PerformUntilStatement | PerformVaryingStatement | PerformAfterStatement | ReadStatement | ReleaseStatement | ReturnStatement | RewriteStatement | SearchStatement | SetStatement | SortStatement | StartStatement | StopStatement
//     | StringStatement | SubtractStatement | UnstringStatement | WriteStatement;
export type CompilerDirectingStatement =
  | BasisStatement
  | CblProcessStatement
  | ControlCblStatement
  | CopyStatement
  | DeleteCompilerDirectingStatement
  | EjectStatement
  | EnterStatement
  | InsertStatement
  | ReadyOrResetTraceStatement
  | ReplaceStatement;
//     | ServiceLabelStatement | ServiceReloadStatement | SkipStatement | TitleStatement | UseStatement;
// export type SimpleCondition = ClassCondition | ConditionNameCondition | RelationCondition | SignCondition | SwitchStatusCondition | NegatedSimpleCondition;
// export type Name = AlphabetName | ClassName | ConditionName | FileName | IndexName | MnemonicName | RecordName | RoutineName | SymbolicCharacter | LibraryName | ProgramName | TextName | ParagraphName | SectionName | ComputerName | LanguageName | EnvironmentName
// | AssignmentName | BasisName;
