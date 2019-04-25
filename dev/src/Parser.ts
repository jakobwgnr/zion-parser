/**
 * @fileoverview Responsible for parsing tokens and creating an AST
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

// const debug = require('debug')('zion-parser:parser');
import { ErrorHandler } from './ErrorHandler/error-handler';

import { Token } from 'zion-lexer';
import { TokenType } from 'zion-lexer';
import * as Nodes from './nodes';
import { Node } from './nodes';

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export class Parser {
  private index: number = -1;
  private currentToken: Token = new Token();
  private currentNodeHasError: boolean = false;
  private nodeList: Node[] = [];
  public tokens: Token[];
  public errorHandler: ErrorHandler;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.errorHandler = new ErrorHandler();
  }

  public execute(): Node[] {
    this.nextToken();
    while (!(this.currentToken.type === TokenType.EOF)) {
      let node: any = '';
      switch (this.currentToken.type) {
        case TokenType.Keyword:
          switch (this.currentToken.value) {
            case 'IDENTIFICATION':
            case 'ID':
              node = this.parseCobolSourceProgram();
              break;
            case 'PROGRAM-ID':
              node = this.parseProgramId();
              break;
            case 'AUTHOR':
            case 'INSTALLATION':
            case 'DATE-WRITTEN':
            case 'DATE-COMPILED':
            case 'SECURITY':
              node = this.parseIdentificationDivisionContent();
              break;
            case 'ENVIRONMENT':
              node = this.parseEnvironmentDivisionContent();
            case 'RECORDING':
              node = this.parseRecordingModeClause();
              break;
            case 'CONFIGURATION':
              node = this.parseConfigurationSection();
              break;
            case 'SOURCE-COMPUTER':
              node = this.parseSourceComputerParagraph();
              break;
            case 'OBJECT-COMPUTER':
              node = this.parseObjectComputerParagraph();
              break;
            case 'SPECIAL-NAMES':
              node = this.parseSpecialNamesParagraph();
              break;
            case 'CURRENCY':
              node = this.parseCurrencySignClause();
              break;
            case 'DECIMAL-POINT':
              node = this.parseDecimalPointClause();
              break;
            case 'ALPHABET':
              node = this.parseAlphabetClause();
              break;
            case 'CLASS':
              node = this.parseClassClause();
              break;
            case 'SYMBOLIC':
              node = this.parseSymbolicCharactersClause();
              break;
            case 'INPUT-OUTPUT':
              node = this.parseInputOutputSection();
              break;
            case 'FILE-CONTROL':
              node = this.parseFileControlParagraph();
              break;
            case 'I-O-CONTROL':
              node = this.parseIOControlParagraph();
              break;
            case 'SELECT':
              node = this.parseSelectClause();
              break;
            case 'ASSIGN':
              node = this.parseAssignClause();
              break;
            case 'RESERVE':
              node = this.parseReserveClause();
              break;
            case 'PADDING':
              node = this.parsePaddingCharacterClause();
              break;
            case 'WHEN-COMPILED':
            case 'ADDRESS':
            case 'LENGTH':
              node = this.parseSpecialRegister();
              break;
            case 'RECORD':
              node = this.parseRecordClause();
              break;
            case 'PASSWORD':
              node = this.parsePasswordClause();
              break;
            case 'ALTERNATE':
              node = this.parseAlternateRecordKeyClause();
              break;
            case 'RELATIVE':
              node = this.parseRelativeKeyClause();
              break;
            case 'FILE':
            case 'STATUS':
              node = this.parseFileStatusClause();
              break;
            default:
              this.errorHandler.unexpectedTokenError(
                this.currentToken,
                'Unknown Keyword',
                new Token('Known Keyword', TokenType.Keyword),
              );
              this.nextToken();
              break;
          }
          break;
        case TokenType.NumericLiteral:
          node = this.parseDataDescriptionEntry();
          break;
        default:
          this.errorHandler.unexpectedTokenError(
            this.currentToken,
            'Expected a Keyword - Got other TokenType',
            new Token('any', TokenType.Keyword),
          );
          this.nextToken();
          break;
      }
      this.nodeList.push(node);
    }

    return this.nodeList;
  }

  // TODO: Not finished
  private parseCobolSourceProgram(): Nodes.CobolSourceProgram {
    const node = this.startNode(this.currentToken);

    this.nextToken();
    this.expectKeyword('DIVISION');
    this.nextToken();
    this.expectTerminator();
    this.nextToken();
    const programId: Nodes.ProgramId = this.parseProgramId();
    const identificationDivisionContent: Nodes.IdentificationDivisionContent = this.parseIdentificationDivisionContent();
    this.expectKeyword('ENVIRONMENT');
    const environmentDivisionContent: Nodes.EnvironmentDivisionContent = this.parseEnvironmentDivisionContent();
    return this.finalizeNode(
      new Nodes.CobolSourceProgram(node, programId, identificationDivisionContent, environmentDivisionContent),
      this.currentToken,
    );
  }

  private parseIdentificationDivisionContent(): Nodes.IdentificationDivisionContent {
    const startNodeInfo = this.startNode(this.currentToken);

    let author: string = '';
    let installation: string = '';
    let dateWritten: string = '';
    let dateCompiled: string = '';
    let security: string = '';

    /* istanbul ignore else */
    if (this.isOptionalKeyword('AUTHOR')) {
      this.nextToken();
      this.skipOptionalTerminator();

      author = this.parseCommentEntry();
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('INSTALLATION')) {
      this.nextToken();

      this.skipOptionalTerminator();

      installation = this.parseCommentEntry();
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-WRITTEN')) {
      this.nextToken();

      this.skipOptionalTerminator();

      dateWritten = this.parseCommentEntry();
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-COMPILED')) {
      this.nextToken();

      this.skipOptionalTerminator();

      dateCompiled = this.parseCommentEntry();
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('SECURITY')) {
      this.nextToken();

      this.skipOptionalTerminator();

      security = this.parseCommentEntry();
    }

    return this.finalizeNode(
      new Nodes.IdentificationDivisionContent(startNodeInfo, author, installation, dateWritten, dateCompiled, security),
      this.currentToken,
    );
  }

  // TODO test missing
  private parseEnvironmentDivisionContent(): Nodes.EnvironmentDivisionContent {
    const startNodeInfo = this.startNode(this.currentToken);
    let configurationSection: Nodes.ConfigurationSection | null = null;
    let inputOutputSection: Nodes.InputOutputSection | null = null;

    this.nextToken();
    this.expectKeyword('DIVISION');
    this.nextToken();
    this.expectTerminator();
    this.nextToken();

    if (this.isOptionalKeyword('CONFIGURATION')) {
      configurationSection = this.parseConfigurationSection();
    }

    if (this.isOptionalKeyword('INPUT-OUTPUT')) {
      inputOutputSection = this.parseInputOutputSection();
    }

    return this.finalizeNode(
      new Nodes.EnvironmentDivisionContent(startNodeInfo, configurationSection, inputOutputSection),
      this.currentToken,
    );
  }

  // TODO: impl + test missing
  private parseConfigurationSection(): Nodes.ConfigurationSection | null {
    const startNodeInfo = this.startNode(this.currentToken);
    let sourceComputerParagraph: Nodes.SourceComputerParagraph | null = null;
    let objectComputerParagraph: Nodes.ObjectComputerParagraph | null = null;
    let specialNamesParagraph: Nodes.SpecialNamesParagraph | null = null;

    this.nextToken();
    this.expectKeyword('SECTION');
    this.nextToken();
    this.expectTerminator();
    this.nextToken();

    if (this.isOptionalKeyword('SOURCE-COMPUTER')) {
      sourceComputerParagraph = this.parseSourceComputerParagraph();
    }

    if (this.isOptionalKeyword('OBJECT-COMPUTER')) {
      objectComputerParagraph = this.parseObjectComputerParagraph();
    }

    if (this.isOptionalKeyword('SPECIAL-NAMES')) {
      specialNamesParagraph = this.parseSpecialNamesParagraph();
    }

    return this.finalizeNode(
      new Nodes.ConfigurationSection(
        startNodeInfo,
        sourceComputerParagraph,
        objectComputerParagraph,
        specialNamesParagraph,
      ),
      this.currentToken,
    );
  }

  private parseSourceComputerParagraph(): Nodes.SourceComputerParagraph {
    const startNodeInfo = this.startNode(this.currentToken);
    let sourceComputerValue: string = '';

    this.nextToken();
    this.expectTerminator();
    this.nextToken();

    if (this.isOptionalIdentifier()) {
      sourceComputerValue = this.currentToken.value;
      this.nextToken();
      if (this.isOptionalKeyword('WITH')) {
        this.nextToken();
        this.expectKeyword('DEBUGGING');
        this.nextToken();
        this.expectKeyword('MODE');
        this.nextToken();
      } else {
        if (this.isOptionalKeyword('DEBUGGING')) {
          this.nextToken();
          this.expectKeyword('MODE');
          this.nextToken();
        }
      }
      this.expectTerminator();
      this.nextToken();
    }
    return this.finalizeNode(new Nodes.SourceComputerParagraph(startNodeInfo, sourceComputerValue), this.currentToken);
  }

  private parseObjectComputerParagraph(): Nodes.ObjectComputerParagraph {
    const startNodeInfo = this.startNode(this.currentToken);
    let objectComputerValue: string = '';
    let memorySizeValue: string = '';
    let sequenceValue: string = '';
    let segmentLimitValue: string = '';

    this.nextToken();
    this.expectTerminator();
    this.nextToken();

    if (this.isOptionalIdentifier()) {
      objectComputerValue = this.currentToken.value;
      this.nextToken();
      if (this.isOptionalKeyword('MEMORY')) {
        this.nextToken();
        if (this.isOptionalKeyword('SIZE')) {
          this.nextToken();
        }
        if (this.expectNumeric()) {
          memorySizeValue = this.currentToken.value;
        }
        this.nextToken();
        this.expectSeveralKeywords(['WORDS', 'CHARACTERS', 'MODULES']);
        this.nextToken();

        if (this.isOptionalKeyword('PROGRAM')) {
          this.nextToken();
          if (this.isOptionalKeyword('COLLATING')) {
            this.nextToken();
          }
          this.expectKeyword('SEQUENCE');
          this.nextToken();
          this.skipOptionalKeyword('IS');
          if (this.expectIdentifier()) {
            sequenceValue = this.currentToken.value;
          }
          this.nextToken();
        }

        if (this.isOptionalKeyword('COLLATING')) {
          this.nextToken();
          this.expectKeyword('SEQUENCE');
          this.nextToken();
          this.skipOptionalKeyword('IS');
          if (this.expectIdentifier()) {
            sequenceValue = this.currentToken.value;
          }
          this.nextToken();
        }

        if (this.isOptionalKeyword('SEQUENCE')) {
          this.nextToken();
          this.skipOptionalKeyword('IS');
          if (this.expectIdentifier()) {
            sequenceValue = this.currentToken.value;
          }
          this.nextToken();
        }

        if (this.isOptionalKeyword('SEGMENT-LIMIT')) {
          this.nextToken();
          this.skipOptionalKeyword('IS');
          if (this.expectIdentifier()) {
            segmentLimitValue = this.currentToken.value;
          }
          this.nextToken();
        }
      }
      this.expectTerminator();
      this.nextToken();
    }

    return this.finalizeNode(
      new Nodes.ObjectComputerParagraph(
        startNodeInfo,
        objectComputerValue,
        memorySizeValue,
        sequenceValue,
        segmentLimitValue,
      ),
      this.currentToken,
    );
  }

  private parseSpecialNamesParagraph(): Nodes.SpecialNamesParagraph {
    const startNodeInfo = this.startNode(this.currentToken);
    const specialNamesParagraphStatusPhrases: Nodes.SpecialNamesParagraphStatusPhrase[] = [];
    const specialNamesParagraphClauses: Nodes.SpecialNamesParagraphClause[] = [];
    let currencySignClause: Nodes.CurrencySignClause | null = null;
    let decimalPointClause: Nodes.DecimalPointClause | null = null;

    this.nextToken();
    this.expectTerminator();
    this.nextToken();

    while (this.isOptionalIdentifier() || this.isOptionalKeyword('ON') || this.isOptionalKeyword('OFF')) {
      specialNamesParagraphStatusPhrases.push(this.parseSpecialNamesStatusPhrase());
    }

    while (
      this.isOptionalKeyword('ALPHABET') ||
      this.isOptionalKeyword('SYMBOLIC') ||
      this.isOptionalKeyword('CLASS') ||
      this.isOptionalKeyword('CURRENCY') ||
      this.isOptionalKeyword('DECIMAL-POINT')
    ) {
      if (this.isOptionalKeyword('CURRENCY')) {
        if (currencySignClause === null) {
          currencySignClause = this.parseCurrencySignClause();
        } else {
          this.errorHandler.unexpectedTokenError(
            this.currentToken,
            'Already parsed one currencySignClause - No multiple allowed',
          );
          this.nextToken();
        }
      }

      if (this.isOptionalKeyword('DECIMAL-POINT')) {
        if (decimalPointClause === null) {
          decimalPointClause = this.parseDecimalPointClause();
        } else {
          this.errorHandler.unexpectedTokenError(
            this.currentToken,
            'Already parsed one decimalPointClause - No multiple allowed',
          );
          this.nextToken();
        }
      }

      if (this.isOptionalKeyword('ALPHABET')) {
        specialNamesParagraphClauses.push(this.parseAlphabetClause());
      }

      if (this.isOptionalKeyword('SYMBOLIC')) {
        specialNamesParagraphClauses.push(this.parseSymbolicCharactersClause());
      }

      if (this.isOptionalKeyword('CLASS')) {
        specialNamesParagraphClauses.push(this.parseClassClause());
      }
    }

    return this.finalizeNode(
      new Nodes.SpecialNamesParagraph(
        startNodeInfo,
        specialNamesParagraphStatusPhrases,
        specialNamesParagraphClauses,
        currencySignClause,
        decimalPointClause,
      ),
      this.currentToken,
    );
  }

  private parseSpecialNamesStatusPhrase(): Nodes.SpecialNamesParagraphStatusPhrase {
    const startNodeInfo = this.startNode(this.currentToken);
    let environment: string = '';
    let mnemonic: string = '';
    let onCondition: Nodes.Condition | null = null;
    let offCondition: Nodes.Condition | null = null;

    if (this.isOptionalIdentifier()) {
      environment = this.currentToken.value;
      this.nextToken();
      this.skipOptionalKeyword('IS');
      mnemonic = this.currentToken.value;
      this.nextToken();
    }

    if (this.isOptionalKeyword('ON')) {
      this.nextToken();
      this.skipOptionalKeyword('STATUS');
      this.skipOptionalKeyword('IS');
      onCondition = this.parseCondition();

      if (this.isOptionalKeyword('OFF')) {
        this.nextToken();
        this.skipOptionalKeyword('STATUS');
        this.skipOptionalKeyword('IS');
        offCondition = this.parseCondition();
      }
    }

    if (this.isOptionalKeyword('OFF')) {
      this.nextToken();
      this.skipOptionalKeyword('STATUS');
      this.skipOptionalKeyword('IS');
      offCondition = this.parseCondition();

      if (this.isOptionalKeyword('ON')) {
        this.nextToken();
        this.skipOptionalKeyword('STATUS');
        this.skipOptionalKeyword('IS');
        onCondition = this.parseCondition();
      }
    }

    return this.finalizeNode(
      new Nodes.SpecialNamesParagraphStatusPhrase(startNodeInfo, environment, mnemonic, onCondition, offCondition),
      this.currentToken,
    );
  }

  private parseCurrencySignClause(): Nodes.CurrencySignClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let currencySignValue: string = '';

    this.nextToken();
    this.skipOptionalKeyword('SIGN');
    this.skipOptionalKeyword('IS');
    if (this.expectIdentifier()) {
      currencySignValue = this.currentToken.value;
    }
    this.nextToken();

    return this.finalizeNode(new Nodes.CurrencySignClause(startNodeInfo, currencySignValue), this.currentToken);
  }

  private parseSymbolicCharactersClause(): Nodes.SymbolicCharactersClause {
    const startNodeInfo = this.startNode(this.currentToken);
    const symbolicCharacters: string[] = [];
    const symbolicIntegers: string[] = [];
    let inOrdinalPosition: string = '';

    this.nextToken();
    this.skipOptionalKeyword('CHARACTERS');

    while (this.isOptionalIdentifier()) {
      if (this.expectIdentifier()) {
        while (this.isOptionalIdentifier()) {
          symbolicCharacters.push(this.currentToken.value);
          this.nextToken();
        }
      } else {
        this.nextToken();
      }
      if (this.isSeveralOptionalKeywords(['ARE', 'IS'])) {
        this.nextToken();
      }

      if (this.expectNumeric()) {
        while (this.isOptionalNumeric()) {
          symbolicIntegers.push(this.currentToken.value);
          this.nextToken();
        }
      } else {
        this.nextToken();
      }
    }

    if (this.isOptionalKeyword('IN')) {
      this.nextToken();
      if (this.expectIdentifier()) {
        inOrdinalPosition = this.currentToken.value;
      }
      this.nextToken();
    }

    return this.finalizeNode(
      new Nodes.SymbolicCharactersClause(startNodeInfo, symbolicCharacters, symbolicIntegers, inOrdinalPosition),
      this.currentToken,
    );
  }

  private parseDecimalPointClause(): Nodes.DecimalPointClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.nextToken();
    this.skipOptionalKeyword('IS');
    this.expectKeyword('COMMA');
    this.nextToken();

    return this.finalizeNode(new Nodes.DecimalPointClause(startNodeInfo), this.currentToken);
  }

  private parseAlphabetClause(): Nodes.AlphabetClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let alphabetName: string = '';
    let alphabetType: string = '';
    const alphabetLiterals: Nodes.AlphabetLiteral[] = [];

    this.nextToken();
    if (this.expectIdentifier()) {
      alphabetName = this.currentToken.value;
    }
    this.nextToken();

    this.skipOptionalKeyword('IS');

    if (this.isSeveralOptionalKeywords(['STANDARD-1', 'STANDARD-2', 'NATIVE', 'EBCDIC'])) {
      alphabetType = this.currentToken.value;
      this.nextToken();
    } else {
      if (this.expectIdentifier()) {
        while (this.isOptionalIdentifier()) {
          const alphabetLiteral: Nodes.AlphabetLiteral = new Nodes.AlphabetLiteral();
          alphabetLiteral.literal = this.currentToken.value;
          this.nextToken();
          if (this.isSeveralOptionalKeywords(['THROUGH', 'THRU'])) {
            this.nextToken();
            if (this.expectIdentifier()) {
              alphabetLiteral.throughLiteral = this.currentToken.value;
            }
            this.nextToken();
          }

          if (this.isOptionalKeyword('ALSO')) {
            this.nextToken();
            if (this.expectIdentifier()) {
              alphabetLiteral.alsoLiteral = this.currentToken.value;
            }
            this.nextToken();
          }
          alphabetLiterals.push(alphabetLiteral);
        }
      } else {
        this.nextToken();
      }
    }

    return this.finalizeNode(
      new Nodes.AlphabetClause(startNodeInfo, alphabetName, alphabetType, alphabetLiterals),
      this.currentToken,
    );
  }

  private parseClassClause(): Nodes.ClassClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let className: string = '';
    const classLiterals: Nodes.AlphabetLiteral[] = [];

    this.nextToken();
    if (this.expectIdentifier()) {
      className = this.currentToken.value;
    }
    this.nextToken();

    this.skipOptionalKeyword('IS');

    while (this.isOptionalIdentifier()) {
      const classLiteral: Nodes.AlphabetLiteral = new Nodes.AlphabetLiteral();
      classLiteral.literal = this.currentToken.value;
      this.nextToken();
      if (this.isSeveralOptionalKeywords(['THROUGH', 'THRU'])) {
        this.nextToken();
        if (this.expectIdentifier()) {
          classLiteral.throughLiteral = this.currentToken.value;
        }
        this.nextToken();
      }

      classLiterals.push(classLiteral);
    }

    return this.finalizeNode(new Nodes.ClassClause(startNodeInfo, className, classLiterals), this.currentToken);
  }

  // TODO ALL
  private parseCondition(): null {
    this.nextToken();
    return null;
  }

  // TODO Impl + test missing
  private parseInputOutputSection(): Nodes.InputOutputSection | null {
    const startNodeInfo = this.startNode(this.currentToken);
    let fileControlParagraph: Nodes.FileControlParagraph | null = null;
    let ioControlParagraph: Nodes.IOControlParagraph | null = null;

    this.nextToken();
    this.expectKeyword('SECTION');
    this.nextToken();
    this.expectTerminator();
    this.nextToken();

    if (this.isOptionalKeyword('FILE-CONTROL')) {
      fileControlParagraph = this.parseFileControlParagraph();
    }

    if (this.isOptionalKeyword('I-O-CONTROL')) {
      ioControlParagraph = this.parseIOControlParagraph();
    }

    return this.finalizeNode(
      new Nodes.InputOutputSection(startNodeInfo, fileControlParagraph, ioControlParagraph),
      this.currentToken,
    );
  }

  // TODO Impl + test missing
  private parseFileControlParagraph(): Nodes.FileControlParagraph | null {
    const startNodeInfo = this.startNode(this.currentToken);
    const fileControlEntries: Nodes.FileControlEntry[] = [];

    this.nextToken();
    this.expectTerminator();
    this.nextToken();

    while (this.isOptionalKeyword('SELECT')) {
      fileControlEntries.push(this.parseFileControlEntry());
    }

    return this.finalizeNode(new Nodes.FileControlParagraph(startNodeInfo, fileControlEntries), this.currentToken);
  }

  // TODO Impl + test missing
  private parseFileControlEntry(): Nodes.FileControlEntry {
    const startNodeInfo = this.startNode(this.currentToken);

    const selectClause = this.parseSelectClause();

    const assignClause = this.parseAssignClause();

    return this.finalizeNode(new Nodes.FileControlEntry(startNodeInfo, selectClause, assignClause), this.currentToken);
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:select-clause

  private parseSelectClause(): Nodes.SelectClause | null {
    const startNodeInfo = this.startNode(this.currentToken);

    this.nextToken();

    this.skipOptionalKeyword('OPTIONAL');

    const fileName: string = this.parseFileName();

    return this.finalizeNode(new Nodes.SelectClause(startNodeInfo, fileName), this.currentToken);
  }

  private parseAssignClause(): Nodes.AssignClause | null {
    const startNodeInfo = this.startNode(this.currentToken);
    const assignmentName: string[] = [];

    this.nextToken();

    this.skipOptionalKeyword('TO');
    while (this.isOptionalIdentifier()) {
      assignmentName.push(this.currentToken.value);
      this.nextToken();
    }

    return this.finalizeNode(new Nodes.AssignClause(startNodeInfo, assignmentName), this.currentToken);
  }

  private parseReserveClause(): Nodes.ReserveClause | null {
    const startNodeInfo = this.startNode(this.currentToken);
    let reserveAreaCount: string = '';

    this.nextToken();

    if (this.expectNumeric()) {
      reserveAreaCount = this.currentToken.value;
    }
    this.nextToken();

    this.skipOptionalKeywords(['AREA', 'AREAS']);

    return this.finalizeNode(new Nodes.ReserveClause(startNodeInfo, reserveAreaCount), this.currentToken);
  }

  private parsePaddingCharacterClause(): Nodes.PaddingCharacterClause | null {
    const startNodeInfo = this.startNode(this.currentToken);
    this.nextToken();

    this.skipOptionalKeyword('CHARACTER');
    this.skipOptionalKeyword('IS');

    const qualifiedDataName: Nodes.QualifiedDataName = this.parseQualifiedDataName();

    return this.finalizeNode(new Nodes.PaddingCharacterClause(startNodeInfo, qualifiedDataName), this.currentToken);
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:special-register

  private isSpecialRegister(): boolean {
    return (
      this.currentToken.type === TokenType.Keyword &&
      (this.currentToken.value === 'ADDRESS' ||
        this.currentToken.value === 'DEBUG-ITEM' ||
        this.currentToken.value === 'LENGTH' ||
        this.currentToken.value === 'RETURN-CODE' ||
        this.currentToken.value === 'SHIFT-OUT' ||
        this.currentToken.value === 'SHIFT-IN' ||
        this.currentToken.value === 'SORT-CONTROL' ||
        this.currentToken.value === 'SORT-CORE-SIZE' ||
        this.currentToken.value === 'SORT-FILE-SIZE' ||
        this.currentToken.value === 'SORT-MESSAGE' ||
        this.currentToken.value === 'SORT-MODE-SIZE' ||
        this.currentToken.value === 'SORT-RETURN' ||
        this.currentToken.value === 'TALLY' ||
        this.currentToken.value === 'WHEN-COMPILED')
    );
  }

  private parseSpecialRegister(): Nodes.SpecialRegister {
    const startNodeInfo = this.startNode(this.currentToken);
    let optionalValue: string = '';
    const specialRegisterType: string = this.currentToken.value;
    this.nextToken();

    if (specialRegisterType === 'ADDRESS' || specialRegisterType === 'LENGTH') {
      this.skipOptionalKeyword('OF');
      optionalValue = this.currentToken.value;
      this.nextToken();
    }

    return this.finalizeNode(
      new Nodes.SpecialRegister(startNodeInfo, specialRegisterType, optionalValue),
      this.currentToken,
    );
  }
  private parseRecordClause(): Nodes.RecordDelimiterClause | Nodes.RecordKeyClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let value: string = '';

    this.nextToken();

    if (this.isOptionalKeyword('DELIMITER')) {
      this.nextToken();
      this.skipOptionalKeyword('IS');
      value = this.currentToken.value;
      this.nextToken();

      return this.finalizeNode(new Nodes.RecordDelimiterClause(startNodeInfo, value), this.currentToken);
    } else {
      const dataNames: string[] = [];
      this.skipOptionalKeyword('KEY');
      this.skipOptionalKeyword('IS');

      if (this.expectIdentifier()) {
        value = this.currentToken.value;
      }
      this.nextToken();

      while (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
        this.skipOptionalKeywords(['IN', 'OF']);
        dataNames.push(this.currentToken.value);
        this.nextToken();
      }

      return this.finalizeNode(new Nodes.RecordKeyClause(startNodeInfo, value, dataNames), this.currentToken);
    }
  }

  private parseAlternateRecordKeyClause(): Nodes.AlternateRecordKeyClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let value: string = '';
    const dataNames: string[] = [];
    let passwordClause: Nodes.PasswordClause | null = null;

    this.nextToken();
    this.skipOptionalKeyword('RECORD');
    this.skipOptionalKeyword('KEY');
    this.skipOptionalKeyword('IS');

    if (this.expectIdentifier()) {
      value = this.currentToken.value;
    }
    this.nextToken();

    while (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
      this.skipOptionalKeywords(['IN', 'OF']);
      dataNames.push(this.currentToken.value);
      this.nextToken();
    }
    if (this.isOptionalKeyword('PASSWORD')) {
      passwordClause = this.parsePasswordClause();
    }
    this.skipOptionalKeyword('WITH');
    this.skipOptionalKeyword('DUPLICATES');

    return this.finalizeNode(
      new Nodes.AlternateRecordKeyClause(startNodeInfo, value, dataNames, passwordClause),
      this.currentToken,
    );
  }

  private parseRelativeKeyClause(): Nodes.RelativeKeyClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let value: string = '';
    const dataNames: string[] = [];

    this.nextToken();
    this.skipOptionalKeyword('KEY');
    this.skipOptionalKeyword('IS');

    if (this.expectIdentifier()) {
      value = this.currentToken.value;
    }
    this.nextToken();

    while (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
      this.skipOptionalKeywords(['IN', 'OF']);
      dataNames.push(this.currentToken.value);
      this.nextToken();
    }

    return this.finalizeNode(new Nodes.RelativeKeyClause(startNodeInfo, value, dataNames), this.currentToken);
  }

  private parseFileStatusClause(): Nodes.RelativeKeyClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let value: string = '';
    const dataNames: string[] = [];
    let optionalValue: string = '';
    const optionalDataNames: string[] = [];

    this.skipOptionalKeyword('FILE');
    this.nextToken();

    this.skipOptionalKeyword('IS');

    if (this.expectIdentifier()) {
      value = this.currentToken.value;
    }
    this.nextToken();

    while (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
      this.skipOptionalKeywords(['IN', 'OF']);
      dataNames.push(this.currentToken.value);
      this.nextToken();
    }

    if (this.isOptionalIdentifier()) {
      optionalValue = this.currentToken.value;
    }
    this.nextToken();

    while (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
      this.skipOptionalKeywords(['IN', 'OF']);
      optionalDataNames.push(this.currentToken.value);
      this.nextToken();
    }

    return this.finalizeNode(
      new Nodes.FileStatusClause(startNodeInfo, value, dataNames, optionalValue, optionalDataNames),
      this.currentToken,
    );
  }

  private parsePasswordClause(): Nodes.PasswordClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let dataName: string = '';

    this.nextToken();
    this.skipOptionalKeyword('IS');

    if (this.expectIdentifier()) {
      dataName = this.currentToken.value;
    }
    this.nextToken();

    return this.finalizeNode(new Nodes.PasswordClause(startNodeInfo, dataName), this.currentToken);
  }

  // TODO Impl + test missing
  private parseIOControlParagraph(): Nodes.IOControlParagraph | null {
    const startNodeInfo = this.startNode(this.currentToken);

    return this.finalizeNode(new Nodes.IOControlParagraph(startNodeInfo), this.currentToken);
  }

  private parseRecordingModeClause(): Nodes.RecordingModeClause {
    const startNodeInfo = this.startNode(this.currentToken);
    let recordingMode: string = '';
    this.nextToken();

    this.skipOptionalKeyword('MODE');
    this.skipOptionalKeyword('IS');

    if (this.expectModeIdentifier()) {
      recordingMode = this.currentToken.value;
    }
    this.nextToken();

    return this.finalizeNode(new Nodes.RecordingModeClause(startNodeInfo, recordingMode), this.currentToken);
  }

  private parseProgramId(): Nodes.ProgramId {
    const startNodeInfo = this.startNode(this.currentToken);
    let initialRequired: boolean = false;
    let initialExists: boolean = false;
    let programIdValue: string = '';

    this.nextToken();
    this.skipOptionalTerminator();

    if (this.expectIdentifier()) {
      programIdValue = this.currentToken.value;
    }
    this.nextToken();

    /* istanbul ignore else */
    if (this.isOptionalKeyword('IS')) {
      initialRequired = true;
      this.nextToken();
    }

    /* istanbul ignore else */
    if (initialRequired) {
      if (this.expectKeyword('INITIAL')) {
        initialExists = true;
      }
      this.nextToken();
    } else {
      if (this.isOptionalKeyword('INITIAL')) {
        initialExists = true;
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('PROGRAM')) {
      if (!initialExists) {
        this.errorHandler.unexpectedTokenError(
          this.currentToken,
          'Keyword "PROGRAM" in the ProgramID statement only allowed if "INITIAL" Keyword is present',
        );
      }
      this.nextToken();
    }

    this.skipOptionalTerminator();

    return this.finalizeNode(new Nodes.ProgramId(startNodeInfo, programIdValue), this.currentToken);
  }

  // --------------------------
  // HELPERS
  // --------------------------
  private nextToken() {
    if (!(this.currentToken.type === TokenType.EOF)) {
      this.index++;
      this.currentToken = this.tokens[this.index];
      if (this.currentToken && this.isIrrelevantToken(this.currentToken)) {
        this.nextToken();
      }
    }
  }

  private isIrrelevantToken(token: Token) {
    return (
      token.type === TokenType.Comment ||
      token.type === TokenType.IdentificationArea ||
      token.type === TokenType.SequenceNumberLiteral ||
      token.type === TokenType.WhiteSpace
    );
  }

  private startNode(token: Token): Nodes.NodeStandardInfo {
    this.currentNodeHasError = false;
    return {
      startColumnTotal: token.startColumnTotal,
      startColumnRelative: token.startColumnRelative,
      startLine: token.startLine,
    };
  }

  private finalizeNode(node: any, endToken: Token) {
    node.endColumnRelative = endToken.endColumnRelative;
    node.endColumnTotal = endToken.endColumnTotal;
    node.endLine = endToken.endLine;
    node.hasError = this.currentNodeHasError;

    this.currentNodeHasError = false;

    return node;
  }

  private setCurrentNodeHasError() {
    this.currentNodeHasError = true;
  }

  private expectKeyword(keyword: string): boolean {
    if (this.currentToken.type !== TokenType.Keyword || this.currentToken.value !== keyword) {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected certain type of keyword. Got different',
        new Token(keyword, TokenType.Keyword),
      );
      this.setCurrentNodeHasError();
      return false;
    }
    return true;
  }

  /* istanbul ignore next */
  private expectSeveralKeywords(keywords: string[]): boolean {
    if (this.currentToken.type !== TokenType.Keyword || !keywords.includes(this.currentToken.value)) {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected one of the keywords in the list. Got different',
        new Token(keywords.toString(), TokenType.Keyword),
      );
      this.setCurrentNodeHasError();
      return false;
    }
    return true;
  }

  /* istanbul ignore next */
  private expectModeIdentifier(): boolean {
    if (
      this.currentToken.type !== TokenType.Identifier ||
      !(
        this.currentToken.value === 'F' ||
        this.currentToken.value === 'V' ||
        this.currentToken.value === 'U' ||
        this.currentToken.value === 'S'
      )
    ) {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected special type of Identifier',
        new Token('F or V or U or S', 'ModeIdentifier'),
      );
      this.setCurrentNodeHasError();
      return false;
    }
    return true;
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:compiler-directing-statement

  // private parseCompilerDirectingStatement(): Nodes.CompilerDirectingStatement | void {
  //   this.expectCompilerDirectingStatement();

  //   if (this.isBasisStatement()) {
  //     //
  //   } else {
  //     if (this.isCblProcessStatement()) {
  //       //
  //     } else {
  //       if (this.isControlCblStatement()) {
  //         //
  //       } else {
  //         if (this.isCopyStatement()) {
  //           return this.parseCopyStatement();
  //         } else {
  //           if (this.isDeleteCompilerDirectingStatement()) {
  //             //
  //           } else {
  //             if (this.isEjectStatement()) {
  //               return this.parseEjectStatement();
  //             } else {
  //               if (this.isEnterStatement()) {
  //                 return this.parseEnterStatement();
  //               } else {
  //                 if (this.isInsertStatement()) {
  //                   //
  //                 } else {
  //                   if (this.isReadyOrResetStatement()) {
  //                     return this.parseReadyOrResetStatement();
  //                   } else {
  //                     if (this.isReplaceStatement()) {
  //                       return this.parseReplaceStatement();
  //                     } else {
  //                       if (this.isServiceStatement()) {
  //                         return this.parseServiceStatement();
  //                       } else {
  //                         if (this.isSkipStatement()) {
  //                           return this.parseSkipStatement();
  //                         } else {
  //                           if (this.isTitleStatement()) {
  //                             return this.parseTitleStatement();
  //                           } else {
  //                             if (this.isUseStatement()) {
  //                               //
  //                             }
  //                           }
  //                         }
  //                       }
  //                     }
  //                   }
  //                 }
  //               }
  //             }
  //           }
  //         }
  //       }
  //     }
  //   }
  // }

  // private isCompilerDirectingStatement(): boolean {
  //   return (
  //     this.isBasisStatement() ||
  //     this.isCblProcessStatement() ||
  //     this.isControlCblStatement() ||
  //     this.isCopyStatement() ||
  //     this.isDeleteCompilerDirectingStatement() ||
  //     this.isEjectStatement() ||
  //     this.isEnterStatement() ||
  //     this.isInsertStatement() ||
  //     this.isReadyOrResetStatement() ||
  //     this.isReplaceStatement() ||
  //     this.isServiceStatement() ||
  //     this.isSkipStatement() ||
  //     this.isTitleStatement() ||
  //     this.isUseStatement()
  //   );
  // }

  // private expectCompilerDirectingStatement(): boolean {
  //   if (this.isCompilerDirectingStatement()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Compiler Directing Statement Statement - Got different',
  //       new Token(undefined, TokenType.Keyword),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:basis-statement
  // TODO

  // private isBasisStatement(): boolean {
  //   return this.isOptionalKeyword('BASIS');
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:cbl-process-statement
  // // TODO

  // private isCblProcessStatement(): boolean {
  //   return this.isOptionalKeyword('CBL') || this.isOptionalKeyword('PROCESS');
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:control-cbl-statement
  // // TODO

  // private isControlCblStatement(): boolean {
  //   return this.isOptionalKeyword('CONTROL') || this.isOptionalKeyword('CBL');
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:copy-statement

  // private parseCopyStatement(): Nodes.CopyStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);
  //   let value: string | Nodes.Literal = '';
  //   let libraryName: string | Nodes.Literal = '';
  //   const replacingCopyOperand: Nodes.CopyOperand[] = [];
  //   const byCopyOperand: Nodes.CopyOperand[] = [];

  //   this.expectCopyStatement();
  //   this.nextToken();

  //   if (this.isTextName()) {
  //     value = this.currentToken.value;
  //     this.nextToken();
  //   } else {
  //     this.expectLiteral();
  //     value = this.parseLiteral();
  //   }

  //   if (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
  //     this.nextToken();
  //     if (this.isLibraryName()) {
  //       libraryName = this.currentToken.value;
  //       this.nextToken();
  //     } else {
  //       this.expectLiteral();
  //       libraryName = this.parseLiteral();
  //     }
  //   }

  //   this.skipOptionalKeyword('SUPPRESS');

  //   if (this.isOptionalKeyword('REPLACING')) {
  //     this.nextToken();

  //     while (this.isCopyOperand()) {
  //       replacingCopyOperand.push(this.parseCopyOperand());
  //       this.expectKeyword('BY');
  //       this.nextToken();
  //       byCopyOperand.push(this.parseCopyOperand());
  //     }
  //   }
  //   this.expectTerminator();
  //   this.nextToken();

  //   return this.finalizeNode(
  //     new Nodes.CopyStatement(startNodeInfo, value, libraryName, replacingCopyOperand, byCopyOperand),
  //     this.currentToken,
  //   );
  // }

  // private isCopyStatement(): boolean {
  //   return this.isOptionalKeyword('COPY');
  // }

  // private expectCopyStatement(): boolean {
  //   if (this.isCopyStatement()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Copy Statement - Got different',
  //       new Token(undefined, TokenType.Keyword),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:delete-compiler-directing-statement
  // // TODO

  // private isDeleteCompilerDirectingStatement(): boolean {
  //   return this.isOptionalKeyword('DELETE');
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:eject-statement

  // private parseEjectStatement(): Nodes.EjectStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);

  //   this.expectEjectStatement();
  //   this.nextToken();
  //   this.skipOptionalTerminator();

  //   return this.finalizeNode(new Nodes.EjectStatement(startNodeInfo), this.currentToken);
  // }

  // private isEjectStatement(): boolean {
  //   return this.isOptionalKeyword('EJECT');
  // }

  // private expectEjectStatement(): boolean {
  //   if (this.isEjectStatement()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Eject Statement - Got different',
  //       new Token(undefined, TokenType.Keyword),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:enter-statement

  // private parseEnterStatement(): Nodes.EnterStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);
  //   let routineName: string = '';

  //   this.expectEnterStatement();
  //   this.nextToken();
  //   const languageName: string = this.currentToken.value;
  //   this.nextToken();
  //   if (this.isRoutineName()) {
  //     routineName = this.currentToken.value;
  //     this.nextToken();
  //   }
  //   this.expectTerminator();
  //   this.nextToken();

  //   return this.finalizeNode(new Nodes.EnterStatement(startNodeInfo, languageName, routineName), this.currentToken);
  // }

  // private isEnterStatement(): boolean {
  //   return this.isOptionalKeyword('ENTER');
  // }

  // private expectEnterStatement(): boolean {
  //   if (this.isEnterStatement()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Enter Statement - Got different',
  //       new Token(undefined, TokenType.Keyword),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:insert-statement
  // // TODO

  // private isInsertStatement(): boolean {
  //   return this.isOptionalKeyword('INSERT');
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:ready-or-reset-trace-statement

  // private parseReadyOrResetStatement(): Nodes.ReadyOrResetTraceStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);

  //   this.expectReadyOrResetStatement();
  //   this.nextToken();
  //   this.expectKeyword('TRACE');
  //   this.nextToken();
  //   this.expectTerminator();
  //   this.nextToken();

  //   return this.finalizeNode(new Nodes.ReadyOrResetTraceStatement(startNodeInfo), this.currentToken);
  // }

  // private isReadyOrResetStatement(): boolean {
  //   return this.isOptionalKeyword('READY') || this.isOptionalKeyword('RESET');
  // }

  // private expectReadyOrResetStatement(): boolean {
  //   if (this.isReadyOrResetStatement()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Ready or Reset Statement - Got different',
  //       new Token(undefined, TokenType.Keyword),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:replace-statement

  // private parseReplaceStatement(): Nodes.ReplaceStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);
  //   const quotedPseudoText: Nodes.QuotedPseudoText[] = [];
  //   const byQuotedPseudoText: Nodes.QuotedPseudoText[] = [];

  //   this.nextToken();

  //   if (this.isOptionalKeyword('OFF')) {
  //     this.nextToken();
  //   } else {
  //     this.expectQuotedPseudoText();
  //     while (this.isQuotedPseudoText()) {
  //       quotedPseudoText.push(this.parseQuotedPseudoText());
  //       this.expectKeyword('BY');
  //       this.nextToken();
  //       byQuotedPseudoText.push(this.parseQuotedPseudoText());
  //     }
  //     this.expectTerminator();
  //     this.nextToken();
  //   }
  //   return this.finalizeNode(
  //     new Nodes.ReplaceStatement(startNodeInfo, quotedPseudoText, byQuotedPseudoText),
  //     this.currentToken,
  //   );
  // }

  // private isReplaceStatement(): boolean {
  //   return this.isOptionalKeyword('REPLACE');
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:service-reload-statement
  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:service-label-statement

  // private parseServiceStatement(): Nodes.ServiceStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);

  //   this.nextToken();

  //   if (this.isOptionalKeyword('LABEL')) {
  //     this.nextToken();
  //     return this.finalizeNode(new Nodes.ServiceLabelStatement(startNodeInfo), this.currentToken);
  //   } else {
  //     this.nextToken();
  //     const identifier: Nodes.Identifier = this.parseIdentifier();
  //     return this.finalizeNode(new Nodes.ServiceReloadStatement(startNodeInfo, identifier), this.currentToken);
  //   }
  // }

  // private isServiceStatement(): boolean {
  //   return this.isOptionalKeyword('SERVICE');
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:skip-statement

  // private parseSkipStatement(): Nodes.SkipStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);

  //   this.nextToken();
  //   this.skipOptionalTerminator();
  //   return this.finalizeNode(new Nodes.SkipStatement(startNodeInfo), this.currentToken);
  // }

  // private isSkipStatement(): boolean {
  //   return this.isSeveralOptionalKeywords(['SKIP1', 'SKIP2', 'SKIP3']);
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:title-statement

  // private parseTitleStatement(): Nodes.TitleStatement {
  //   const startNodeInfo = this.startNode(this.currentToken);

  //   this.nextToken();
  //   const literal: Nodes.Literal = this.parseLiteral();
  //   this.skipOptionalTerminator();
  //   return this.finalizeNode(new Nodes.TitleStatement(startNodeInfo, literal), this.currentToken);
  // }

  // private isTitleStatement(): boolean {
  //   return this.isOptionalKeyword('TITLE');
  // }

  // private expectTitleStatement(): boolean {
  //   if (this.isTitleStatement()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Title Statement - Got different',
  //       new Token(undefined, TokenType.Keyword),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:use-statement

  // private isUseStatement(): boolean {
  //   return this.isOptionalKeyword('USE');
  // }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:identifier

  // private parseIdentifier(): Nodes.Identifier {
  //   if (this.isOptionalKeyword('LINAGE-COUNTER')) {
  //     return this.parseLinageCounterIdentifier();
  //   } else {
  //     return this.parseQualifiedIdentifier();
  //   }
  // }

  // private parseLinageCounterIdentifier(): Nodes.LinageCounterIdentifier {
  //   const startNodeInfo = this.startNode(this.currentToken);
  //   let fileName: string = '';
  //   this.nextToken();
  //   if (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
  //     this.nextToken();
  //     fileName = this.currentToken.value;
  //     this.nextToken();
  //   }
  //   return this.finalizeNode(new Nodes.LinageCounterIdentifier(startNodeInfo, fileName), this.currentToken);
  // }

  // private parseQualifiedIdentifier(): Nodes.QualifiedIdentifier {
  //   const startNodeInfo = this.startNode(this.currentToken);
  //   // const qualifiedDateName: Nodes.QualifiedDataName = this.parseQualifiedDataName();
  //   // const subscripts: Nodes.Subscript[] = [];
  //   // let leftMostCharacterPosition: Nodes.LeftMostCharacterPosition;

  //   // while (this.isSubscript()) {
  //   //   subscripts.push(this.parseSubscript());
  //   // }

  //   // if (this.isBracketToken()) {
  //   //   this.nextToken();
  //   //   leftMostCharacterPosition = this.parseLeftMostCharacterPosition();
  //   // }

  //   return this.finalizeNode(new Nodes.QualifiedIdentifier(startNodeInfo), this.currentToken);
  // }

  // private isCobolIdentifier(): boolean {
  //   return this.isQualifiedDataName() || this.isOptionalKeyword('LINAGE-COUNTER');
  // }

  // // private expectCobolIdentifier(): boolean {
  // //   if (this.isCobolIdentifier()) {
  // //     return true;
  // //   } else {
  // //     this.errorHandler.unexpectedTokenError(
  // //       this.currentToken,
  // //       'Expected Identifier - Got different',
  // //       new Token(undefined, TokenType.Identifier),
  // //     );
  // //     this.setCurrentNodeHasError();

  // //     return false;
  // //   }
  // // }

  // // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:literal

  private parseLiteral(): Nodes.Literal {
    const startNodeInfo = this.startNode(this.currentToken);
    let value: string | Nodes.FigurativeConstant;

    this.expectLiteral();

    if (this.isFigurativeConstant()) {
      value = this.parseFigurativeConstant();
    } else {
      value = this.currentToken.value;
      this.nextToken();
    }

    return this.finalizeNode(new Nodes.Literal(startNodeInfo, value), this.currentToken);
  }

  private isLiteral(): boolean {
    return this.isNumeric() || this.isNotNumeric() || this.isFigurativeConstant();
  }

  /* istanbul ignore next */
  private expectLiteral(): boolean {
    if (this.isLiteral()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected Literal - Got different',
        new Token(undefined, TokenType.StringLiteral),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:procedure-name

  // private isProcedureName(): boolean {
  //   return this.isParagraphName() || this.isSectionName();
  // }

  // private expectProcedureName(): boolean {
  //   if (this.isProcedureName()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Procedure Name - Got different',
  //       new Token(undefined, TokenType.Identifier),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:qualified-data-name

  private parseQualifiedDataName(): Nodes.QualifiedDataName {
    const startNodeInfo = this.startNode(this.currentToken);
    let value: string | Nodes.SpecialRegister = '';
    const dataNames: string[] = [];

    this.expectQualifiedDataName();

    if (this.isSpecialRegister()) {
      value = this.parseSpecialRegister();
    } else {
      if (this.expectDataName()) {
        value = this.currentToken.value;
      }
      this.nextToken();

      while (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
        this.skipOptionalKeywords(['IN', 'OF']);
        dataNames.push(this.currentToken.value);
        this.nextToken();
      }
    }

    return this.finalizeNode(new Nodes.QualifiedDataName(startNodeInfo, value, dataNames), this.currentToken);
  }

  private isQualifiedDataName(): boolean {
    return this.isDataName() || this.isSpecialRegister();
  }

  /* istanbul ignore next */
  private expectQualifiedDataName(): boolean {
    if (this.isQualifiedDataName()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected Qualified Data Name - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:copy-operand

  //   private parseCopyOperand(): Nodes.CopyOperand {
  //     const startNodeInfo = this.startNode(this.currentToken);
  //     let value: string | Nodes.QuotedPseudoText | Nodes.Identifier | Nodes.Literal = '';

  //     if (this.isQuotedPseudoText()) {
  //       value = this.parseQuotedPseudoText();
  //     } else {
  //       if (this.isCobolIdentifier()) {
  //         value = this.parseIdentifier();
  //       } else {
  //         if (this.isLiteral()) {
  //           value = this.parseLiteral();
  //         } else {
  //           value = this.currentToken.value;
  //           this.nextToken();
  //         }
  //       }
  //     }

  //     return this.finalizeNode(new Nodes.CopyOperand(startNodeInfo, value), this.currentToken);
  //   }

  //   private isCopyOperand(): boolean {
  //     return this.isQuotedPseudoText() || this.isCobolIdentifier() || this.isLiteral() || this.isCobolWord();
  //   }

  //   private expectCopyOperand(): boolean {
  //     if (this.isCopyOperand()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Copy Operand - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:cobol-word

  private parseCobolWord(): string {
    return this.currentToken.value;
  }

  private isCobolWord(): boolean {
    return this.currentToken.type === TokenType.Identifier;
  }

  // private expectCobolWord(): boolean {
  //   if (this.isCobolWord()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Cobol Word - Got different',
  //       new Token(undefined, TokenType.Identifier),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:user-defined-word

  //   private isUserDefinedWord(): boolean {
  //     return this.isCobolWord();
  //   }

  //   private expectUserDefinedWord(): boolean {
  //     if (this.isUserDefinedWord()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected User Defined Word - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:alphabet-name

  //   private isAlphabetName(): boolean {
  //     return this.isAlphabeticUserDefinedWord();
  //   }

  //   private expectAlphabetName(): boolean {
  //     if (this.isAlphabetName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected alphabet Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:class-name

  //   private isClassName(): boolean {
  //     return this.isAlphabeticUserDefinedWord();
  //   }

  //   private expectClassName(): boolean {
  //     if (this.isClassName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Class Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:condition-name

  private parseConditionName(): string {
    this.expectConditionName();
    const value: string = this.currentToken.value;
    this.nextToken();
    return value;
  }

  private isConditionName(): boolean {
    return this.isAlphabeticUserDefinedWord();
  }

  /* istanbul ignore next */
  private expectConditionName(): boolean {
    if (this.isConditionName()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected Condition Name - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:data-name

  private parseDataName(): string {
    this.expectDataName();
    const value: string = this.currentToken.value;
    this.nextToken();
    return value;
  }

  private isDataName(): boolean {
    return this.isAlphabeticUserDefinedWord();
  }

  /* istanbul ignore next */
  private expectDataName(): boolean {
    if (this.isDataName()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected Data Name - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:file-name

  private parseFileName(): string {
    let value: string = '';
    if (this.expectFileName()) {
      value = this.getAlphabeticUserDefinedWord();
      this.nextToken();
    }
    return value;
  }

  private isFileName(): boolean {
    return this.isAlphabeticUserDefinedWord();
  }

  /* istanbul ignore next */
  private expectFileName(): boolean {
    if (this.isFileName()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected File Name - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:index-name

  private parseIndexName(): string {
    this.expectIndexName();
    const value: string = this.currentToken.value;
    this.nextToken();
    return value;
  }

  private isIndexName(): boolean {
    return this.isAlphabeticUserDefinedWord();
  }

  /* istanbul ignore next */
  private expectIndexName(): boolean {
    if (this.isIndexName()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected Index Name - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:mnemonic-name

  //   private isMnemonicName(): boolean {
  //     return this.isAlphabeticUserDefinedWord();
  //   }

  //   private expectMnemonicName(): boolean {
  //     if (this.isMnemonicName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Mnemonic Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:record-name

  //   private isRecordName(): boolean {
  //     return this.isQualifiedDataName();
  //   }

  //   private expectRecordName(): boolean {
  //     if (this.isRecordName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Record Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:routine-name

  //   private isRoutineName(): boolean {
  //     return this.isAlphabeticUserDefinedWord();
  //   }

  //   private expectRoutineName(): boolean {
  //     if (this.isRoutineName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Routine Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:symbolic-character

  //   private isSymbolicCharacter(): boolean {
  //     return this.isAlphabeticUserDefinedWord();
  //   }

  //   private expectSymbolicCharacter(): boolean {
  //     if (this.isSymbolicCharacter()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Symbolic Character - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:library-name

  //   private isLibraryName(): boolean {
  //     return this.isUserDefinedWord();
  //   }

  //   private expectLibraryName(): boolean {
  //     if (this.isLibraryName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Library Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:text-name

  //   private isTextName(): boolean {
  //     return this.isUserDefinedWord();
  //   }

  //   private expectTextName(): boolean {
  //     if (this.isTextName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Text Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:paragraph-name

  //   private isParagraphName(): boolean {
  //     return this.isUserDefinedWord();
  //   }

  //   private expectParagraphName(): boolean {
  //     if (this.isParagraphName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Paragraph Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:section-name

  //   private isSectionName(): boolean {
  //     return this.isUserDefinedWord();
  //   }

  //   private expectSectionName(): boolean {
  //     if (this.isSectionName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Section Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:computer-name

  //   private parseComputerName(): string {
  //     if (this.expectComputerName()) {
  //       return this.parseSystemName();
  //     } else {
  //       return '';
  //     }
  //   }

  //   private isComputerName(): boolean {
  //     return this.isSystemName();
  //   }

  //   private expectComputerName(): boolean {
  //     if (this.isComputerName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Computer Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:language-name

  //   private parseLanguageName(): string {
  //     if (this.expectLanguageName()) {
  //       return this.parseSystemName();
  //     } else {
  //       return '';
  //     }
  //   }

  //   private isLanguageName(): boolean {
  //     return this.isSystemName();
  //   }

  //   private expectLanguageName(): boolean {
  //     if (this.isLanguageName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Language Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:environment-name

  //   private parseEnvironmentName(): string {
  //     if (this.expectEnvironmentName()) {
  //       return this.parseSystemName();
  //     } else {
  //       return '';
  //     }
  //   }

  //   private isEnvironmentName(): boolean {
  //     return this.isSystemName();
  //   }

  //   private expectEnvironmentName(): boolean {
  //     if (this.isEnvironmentName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Environment Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:assignment-name

  //   private parseAssignmentName(): string {
  //     if (this.expectAssignmentName()) {
  //       return this.parseSystemName();
  //     } else {
  //       return '';
  //     }
  //   }

  //   private isAssignmentName(): boolean {
  //     return this.isSystemName();
  //   }

  //   private expectAssignmentName(): boolean {
  //     if (this.isAssignmentName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Assignment Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:basis-name

  //   private isBasisName(): boolean {
  //     return this.isProgramName();
  //   }

  //   private expectBasisName(): boolean {
  //     if (this.isBasisName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Basis Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:program-name

  //   private isProgramName(): boolean {
  //     return this.isUserDefinedWord();
  //   }

  //   private expectProgramName(): boolean {
  //     if (this.isProgramName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Program Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:alphabetic-user-defined-word

  private getAlphabeticUserDefinedWord(): string {
    return this.parseCobolWord();
  }

  private isAlphabeticUserDefinedWord(): boolean {
    return this.isCobolWord() && /[a-zA-Z]/.test(this.currentToken.value);
  }

  // private expectAlphabeticUserDefinedWord(): boolean {
  //   if (this.isAlphabeticUserDefinedWord()) {
  //     return true;
  //   } else {
  //     this.errorHandler.unexpectedTokenError(
  //       this.currentToken,
  //       'Expected Alphabetic User Defined Word - Got different',
  //       new Token(undefined, TokenType.Identifier),
  //     );
  //     this.setCurrentNodeHasError();

  //     return false;
  //   }
  // }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:quoted-pseudo-text

  //   private isQuotedPseudoText(): boolean {
  //     return (
  //       this.currentToken.type === TokenType.Operator &&
  //       this.currentToken.value === '==' &&
  //       this.tokens[this.index + 1].type === TokenType.Identifier &&
  //       this.tokens[this.index + 2].type === TokenType.Operator &&
  //       this.tokens[this.index + 2].value === '=='
  //     );
  //   }

  //   private expectQuotedPseudoText(): boolean {
  //     if (this.isQuotedPseudoText()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected Quoted Pseudo Text - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   private parseQuotedPseudoText(): Nodes.QuotedPseudoText {
  //     const startNodeInfo = this.startNode(this.currentToken);
  //     let value: string = '';
  //     if (this.currentToken.type === TokenType.Operator) {
  //       value = value.concat(this.currentToken.value);
  //     }
  //     this.nextToken();
  //     if (this.currentToken.type === TokenType.Identifier) {
  //       value = value.concat(this.currentToken.value);
  //     }
  //     this.nextToken();
  //     if (this.currentToken.type === TokenType.Operator) {
  //       value = value.concat(this.currentToken.value);
  //     }
  //     this.nextToken();

  //     return this.finalizeNode(new Nodes.QuotedPseudoText(startNodeInfo, value), this.currentToken);
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:system-name

  //   private parseSystemName(): string {
  //     if (this.expectSystemName()) {
  //       return this.parseCobolWord();
  //     } else {
  //       return '';
  //     }
  //   }

  //   private isSystemName(): boolean {
  //     return this.isCobolWord();
  //   }

  //   private expectSystemName(): boolean {
  //     if (this.isSystemName()) {
  //       return true;
  //     } else {
  //       this.errorHandler.unexpectedTokenError(
  //         this.currentToken,
  //         'Expected System Name - Got different',
  //         new Token(undefined, TokenType.Identifier),
  //       );
  //       this.setCurrentNodeHasError();

  //       return false;
  //     }
  //   }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:figurative-constant

  private parseFigurativeConstant(): Nodes.FigurativeConstant {
    const startNodeInfo = this.startNode(this.currentToken);
    let optionalValue: Nodes.Literal | null = null;

    this.expectFigurativeConstant();

    const figurativeConstantType: string = this.currentToken.value;
    this.nextToken();

    if (figurativeConstantType === 'ALL') {
      this.expectLiteral();
      optionalValue = this.parseLiteral();
    }

    return this.finalizeNode(
      new Nodes.FigurativeConstant(startNodeInfo, figurativeConstantType, optionalValue),
      this.currentToken,
    );
  }

  private isFigurativeConstant(): boolean {
    return (
      this.currentToken.type === TokenType.Keyword &&
      (this.currentToken.value === 'ZERO' ||
        this.currentToken.value === 'ZEROS' ||
        this.currentToken.value === 'ZEROES' ||
        this.currentToken.value === 'SPACE' ||
        this.currentToken.value === 'SPACES' ||
        this.currentToken.value === 'HIGH-VALUE' ||
        this.currentToken.value === 'HIGH-VALUES' ||
        this.currentToken.value === 'LOW-VALUE' ||
        this.currentToken.value === 'LOW-VALUES' ||
        this.currentToken.value === 'QUOTE' ||
        this.currentToken.value === 'QUOTES' ||
        this.currentToken.value === 'ALL' ||
        this.currentToken.value === 'NULL' ||
        this.currentToken.value === 'NULLS')
    );
  }

  /* istanbul ignore next */
  private expectFigurativeConstant(): boolean {
    if (this.isFigurativeConstant()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected Figurative Constant - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:level-number

  private parseLevelNumber(): Nodes.LevelNumber {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectLevelNumber();

    const value: string = this.currentToken.value;
    this.nextToken();

    return this.finalizeNode(new Nodes.LevelNumber(startNodeInfo, value), this.currentToken);
  }

  private isLevelNumber(): boolean {
    const num: number = parseInt(this.currentToken.value, 10);
    if ((num >= 1 && num <= 49) || num === 66 || num === 77 || num === 88) {
      return true;
    } else {
      return false;
    }
  }

  /* istanbul ignore next */
  private expectLevelNumber(): boolean {
    if (this.isLevelNumber()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected Level Number - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:data-description-entry

  private parseDataDescriptionEntry(): Nodes.DataDescriptionEntry {
    const startNodeInfo = this.startNode(this.currentToken);

    const level: Nodes.LevelNumber = this.parseLevelNumber();

    if (level.value === '66') {
      const dataName: string = this.parseDataName();
      const renamesClause: Nodes.RenamesClause[] = [];
      renamesClause.push(this.parseRenamesClause());

      this.skipMandatoryTerminator();
      return this.finalizeNode(
        new Nodes.DataDescriptionEntry(startNodeInfo, level, dataName, renamesClause),
        this.currentToken,
      );
    } else {
      if (level.value === '88') {
        const conditionName: string = this.parseConditionName();
        const conditionValueClause: Nodes.ConditionValueClause[] = [];
        conditionValueClause.push(this.parseConditionValueClause());
        this.skipMandatoryTerminator();
        return this.finalizeNode(
          new Nodes.DataDescriptionEntry(startNodeInfo, level, conditionName, conditionValueClause),
          this.currentToken,
        );
      } else {
        let dataName: string = '';
        if (this.isDataName()) {
          dataName = this.parseDataName();
        } else {
          this.expectKeyword('FILLER');
          dataName = this.currentToken.value;
          this.nextToken();
        }
        const dataDescriptionEntryClauses: Nodes.DataDescriptionEntryClause[] = [];
        while (this.isDataDescriptionEntryClause()) {
          dataDescriptionEntryClauses.push(this.parseDataDescriptionEntryClause());
        }
        this.skipMandatoryTerminator();

        return this.finalizeNode(
          new Nodes.DataDescriptionEntry(startNodeInfo, level, dataName, dataDescriptionEntryClauses),
          this.currentToken,
        );
      }
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:data-description-entry-clauses

  private parseDataDescriptionEntryClause(): Nodes.DataDescriptionEntryClause {
    this.expectDataDescriptionEntryClause();

    if (this.isRedefinesClause()) {
      return this.parseRedefinesClause();
    }
    if (this.isBlankWhenZeroClause()) {
      return this.parseBlankWhenZeroClause();
    }
    if (this.isExternalOrGlobalClause()) {
      return this.parseExternalOrGlobalClause();
    }
    if (this.isJustifiedClause()) {
      return this.parseJustifiedClause();
    }
    if (this.isOccursClause()) {
      return this.parseOccursClause();
    }
    if (this.isPictureClause()) {
      return this.parsePictureClause();
    }
    if (this.isSignClause()) {
      return this.parseSignClause();
    }
    if (this.isSyncronizedClause()) {
      return this.parseSyncronizedClause();
    }
    if (this.isUsageClause()) {
      return this.parseUsageClause();
    }

    return this.parseDataValueClause();
  }

  private isDataDescriptionEntryClause(): boolean {
    return (
      this.isRedefinesClause() ||
      this.isBlankWhenZeroClause() ||
      this.isExternalOrGlobalClause() ||
      this.isJustifiedClause() ||
      this.isOccursClause() ||
      this.isPictureClause() ||
      this.isSignClause() ||
      this.isSyncronizedClause() ||
      this.isUsageClause() ||
      this.isDataValueClause()
    );
  }

  /* istanbul ignore next */
  private expectDataDescriptionEntryClause(): boolean {
    if (this.isDataDescriptionEntryClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected DataDescriptionEntryClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:redefines-clause

  private parseRedefinesClause(): Nodes.RedefinesClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectRedefinesClause();
    this.skipOptionalKeyword('REDEFINES');

    const dataName: string = this.parseDataName();

    return this.finalizeNode(new Nodes.RedefinesClause(startNodeInfo, dataName), this.currentToken);
  }

  private isRedefinesClause(): boolean {
    return this.isOptionalKeyword('REDEFINES');
  }

  /* istanbul ignore next */
  private expectRedefinesClause(): boolean {
    if (this.isRedefinesClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected RedefinesClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:blank-when-zero-clause

  private parseBlankWhenZeroClause(): Nodes.BlankWhenZeroClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectBlankWhenZeroClause();
    this.skipOptionalKeyword('BLANK');
    this.skipOptionalKeyword('WHEN');
    this.skipOptionalKeywords(['ZERO', 'ZEROS', 'ZEROES']);

    return this.finalizeNode(new Nodes.BlankWhenZeroClause(startNodeInfo), this.currentToken);
  }

  private isBlankWhenZeroClause(): boolean {
    return this.isOptionalKeyword('BLANK');
  }

  /* istanbul ignore next */
  private expectBlankWhenZeroClause(): boolean {
    if (this.isBlankWhenZeroClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected BlankWhenZeroClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:external-clause
  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:global-clause

  private parseExternalOrGlobalClause(): Nodes.ExternalClause | Nodes.GlobalClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectExternalOrGlobalClause();

    this.skipOptionalKeyword('IS');
    if (this.isOptionalKeyword('EXTERNAL')) {
      this.skipOptionalKeyword('EXTERNAL');
      return this.finalizeNode(new Nodes.ExternalClause(startNodeInfo), this.currentToken);
    } else {
      this.skipOptionalKeyword('GLOBAL');
      return this.finalizeNode(new Nodes.GlobalClause(startNodeInfo), this.currentToken);
    }
  }

  private isExternalOrGlobalClause(): boolean {
    return this.isSeveralOptionalKeywords(['IS', 'EXTERNAL', 'GLOBAL']);
  }

  /* istanbul ignore next */
  private expectExternalOrGlobalClause(): boolean {
    if (this.isExternalOrGlobalClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected ExternalClause or GlobalClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:justified-clause

  private parseJustifiedClause(): Nodes.JustifiedClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectJustifiedClause();
    this.skipOptionalKeywords(['JUSTIFIED', 'JUST']);
    this.skipOptionalKeyword('RIGHT');

    return this.finalizeNode(new Nodes.JustifiedClause(startNodeInfo), this.currentToken);
  }

  private isJustifiedClause(): boolean {
    return this.isSeveralOptionalKeywords(['JUSTIFIED', 'JUST']);
  }

  /* istanbul ignore next */
  private expectJustifiedClause(): boolean {
    if (this.isJustifiedClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected JustifiedClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:occurs-clause

  private parseOccursClause(): Nodes.OccursClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectOccursClause();
    this.skipOptionalKeyword('OCCURS');

    const occursValue: string = this.currentToken.value;
    this.nextToken();

    let occursToValue: string = '';
    if (this.isOptionalKeyword('TO')) {
      this.skipOptionalKeyword('TO');
      occursToValue = this.currentToken.value;
      this.nextToken();
    }

    this.skipOptionalKeyword('TIMES');

    let dependingOnQualifiedDataName: Nodes.QualifiedDataName | null = null;
    if (this.isOptionalKeyword('DEPENDING')) {
      this.skipOptionalKeyword('DEPENDING');
      this.skipOptionalKeyword('ON');
      dependingOnQualifiedDataName = this.parseQualifiedDataName();
    }

    const occursKeys: Nodes.OccursKey[] = [];
    while (this.isSeveralOptionalKeywords(['ASCENDING', 'DESCENDING'])) {
      const orderType: string = this.currentToken.value;
      this.nextToken();
      this.skipOptionalKeyword('KEY');
      this.skipOptionalKeyword('IS');

      const keys: Nodes.QualifiedDataName[] = [];
      while (this.isQualifiedDataName()) {
        keys.push(this.parseQualifiedDataName());
      }
      occursKeys.push({ orderType: orderType, keys: keys });
    }

    const indexNames: string[] = [];
    if (this.isOptionalKeyword('INDEXED')) {
      this.skipOptionalKeyword('INDEXED');
      this.skipOptionalKeyword('BY');

      while (this.isIndexName()) {
        indexNames.push(this.parseIndexName());
      }
    }

    return this.finalizeNode(
      new Nodes.OccursClause(
        startNodeInfo,
        occursValue,
        occursToValue,
        dependingOnQualifiedDataName,
        occursKeys,
        indexNames,
      ),
      this.currentToken,
    );
  }

  private isOccursClause(): boolean {
    return this.isOptionalKeyword('OCCURS');
  }

  /* istanbul ignore next */
  private expectOccursClause(): boolean {
    if (this.isOccursClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected OccursClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:picture-clause

  private parsePictureClause(): Nodes.PictureClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectPictureClause();

    this.skipOptionalKeywords(['PICTURE', 'PIC']);
    this.skipOptionalKeyword('IS');

    const pictureString: Nodes.PictureString = this.parsePictureString();

    return this.finalizeNode(new Nodes.PictureClause(startNodeInfo, pictureString), this.currentToken);
  }

  // TODO - Parsing
  private parsePictureString(): Nodes.PictureString {
    const startNodeInfo = this.startNode(this.currentToken);

    const currency: string = '';
    const picChar: string[] = [];
    const value: string[] = [];
    const punctuation: string[] = [];

    picChar.push(this.currentToken.value);
    this.nextToken();

    return this.finalizeNode(
      new Nodes.PictureString(startNodeInfo, currency, picChar, value, punctuation),
      this.currentToken,
    );
  }

  private isPictureClause(): boolean {
    return this.isSeveralOptionalKeywords(['PICTURE', 'PIC']);
  }

  /* istanbul ignore next */
  private expectPictureClause(): boolean {
    if (this.isPictureClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected PictureClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:sign-clause

  private parseSignClause(): Nodes.SignClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectSignClause();

    this.skipOptionalKeyword('SIGN');
    this.skipOptionalKeyword('IS');

    let value: string = '';
    if (this.expectSeveralKeywords(['LEADING', 'TRAILING'])) {
      value = this.currentToken.value;
      this.nextToken();
    }

    this.skipOptionalKeyword('SEPARATE');
    this.skipOptionalKeyword('CHARACTER');

    return this.finalizeNode(new Nodes.SignClause(startNodeInfo, value), this.currentToken);
  }

  private isSignClause(): boolean {
    return this.isSeveralOptionalKeywords(['SIGN', 'LEADING', 'TRAILING']);
  }

  /* istanbul ignore next */
  private expectSignClause(): boolean {
    if (this.isSignClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected SignClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:synchronized-clause

  private parseSyncronizedClause(): Nodes.SyncronizedClause {
    const startNodeInfo = this.startNode(this.currentToken);
    this.expectSyncronizedClause();

    this.skipOptionalKeywords(['SYNCHRONIZED', 'SYNC']);

    let value: string = '';
    if (this.isSeveralOptionalKeywords(['LEFT', 'RIGHT'])) {
      value = this.currentToken.value;
      this.nextToken();
    }

    return this.finalizeNode(new Nodes.SyncronizedClause(startNodeInfo, value), this.currentToken);
  }

  private isSyncronizedClause(): boolean {
    return this.isSeveralOptionalKeywords(['SYNCHRONIZED', 'SYNC']);
  }

  /* istanbul ignore next */
  private expectSyncronizedClause(): boolean {
    if (this.isSyncronizedClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected SyncronizedClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:usage-clause

  private parseUsageClause(): Nodes.UsageClause {
    const startNodeInfo = this.startNode(this.currentToken);
    this.expectUsageClause();

    this.skipOptionalKeyword('USAGE');
    this.skipOptionalKeyword('IS');

    const value: string = this.currentToken.value;
    this.nextToken();

    return this.finalizeNode(new Nodes.UsageClause(startNodeInfo, value), this.currentToken);
  }

  private isUsageClause(): boolean {
    return this.isSeveralOptionalKeywords([
      'USAGE',
      'BINARY',
      'COMP',
      'COMP-1',
      'COMP-2',
      'COMP-3',
      'COMP-4',
      'COMPUTATIONAL',
      'COMPUTATIONAL-1',
      'COMPUTATIONAL-2',
      'COMPUTATIONAL-3',
      'COMPUTATIONAL-4',
      'DISPLAY',
      'DISPLAY-1',
      'INDEX',
      'PACKED-DECIMAL',
      'POINTER',
    ]);
  }

  /* istanbul ignore next */
  private expectUsageClause(): boolean {
    if (this.isUsageClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected UsageClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:renames-clause

  private parseRenamesClause(): Nodes.RenamesClause {
    const startNodeInfo = this.startNode(this.currentToken);
    this.expectRenamesClause();

    this.skipOptionalKeyword('RENAMES');
    const renamesQualifiedDataName: Nodes.QualifiedDataName = this.parseQualifiedDataName();
    let throughQualifiedDataName: Nodes.QualifiedDataName | null = null;
    if (this.isSeveralOptionalKeywords(['THROUGH', 'THRU'])) {
      this.skipOptionalKeywords(['THROUGH', 'THRU']);
      throughQualifiedDataName = this.parseQualifiedDataName();
    }

    return this.finalizeNode(
      new Nodes.RenamesClause(startNodeInfo, renamesQualifiedDataName, throughQualifiedDataName),
      this.currentToken,
    );
  }

  private isRenamesClause(): boolean {
    return this.isOptionalKeyword('RENAMES');
  }

  /* istanbul ignore next */
  private expectRenamesClause(): boolean {
    if (this.isRenamesClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected RenamesClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:condition-value-clause

  private parseConditionValueClause(): Nodes.ConditionValueClause {
    const startNodeInfo = this.startNode(this.currentToken);
    const literals: Nodes.Literal[] = [];
    const throughLiterals: Nodes.Literal[] = [];

    this.expectConditionValueClause();
    this.nextToken();
    this.skipOptionalKeywords(['IS', 'ARE']);
    while (this.isLiteral()) {
      literals.push(this.parseLiteral());

      if (this.isSeveralOptionalKeywords(['THRU', 'THROUGH'])) {
        this.skipOptionalKeywords(['THRU', 'THROUGH']);
        throughLiterals.push(this.parseLiteral());
      }
    }

    return this.finalizeNode(
      new Nodes.ConditionValueClause(startNodeInfo, literals, throughLiterals),
      this.currentToken,
    );
  }

  private isConditionValueClause(): boolean {
    return this.isSeveralOptionalKeywords(['VALUE', 'VALUES']);
  }

  /* istanbul ignore next */
  private expectConditionValueClause(): boolean {
    if (this.isConditionValueClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected ConditionValueClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:data-value-clause

  private parseDataValueClause(): Nodes.DataValueClause {
    const startNodeInfo = this.startNode(this.currentToken);

    this.expectDataValueClause();
    this.nextToken();
    this.skipOptionalKeyword('IS');
    const value: Nodes.Literal = this.parseLiteral();

    return this.finalizeNode(new Nodes.DataValueClause(startNodeInfo, value), this.currentToken);
  }

  private isDataValueClause(): boolean {
    return this.isOptionalKeyword('VALUE');
  }

  /* istanbul ignore next */
  private expectDataValueClause(): boolean {
    if (this.isDataValueClause()) {
      return true;
    } else {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected DataValueClause - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();

      return false;
    }
  }

  // --------------------
  // TOKEN
  // ----------------------------
  private expectIdentifier(): boolean {
    if (this.currentToken.type !== TokenType.Identifier) {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected identifier token - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      this.setCurrentNodeHasError();
      return false;
    }
    return true;
  }

  private expectNumeric(): boolean {
    if (this.currentToken.type !== TokenType.NumericLiteral) {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected numeric token - Got different',
        new Token(undefined, TokenType.NumericLiteral),
      );
      this.setCurrentNodeHasError();
      return false;
    }
    return true;
  }

  private expectTerminator() {
    if (this.currentToken.type !== TokenType.Terminator) {
      this.errorHandler.unexpectedTokenError(
        this.currentToken,
        'Expected terminator token - Got different',
        new Token(undefined, TokenType.Terminator),
      );
      this.setCurrentNodeHasError();
      return false;
    }
    return true;
  }

  private isOptionalKeyword(keyword: string) {
    return this.currentToken.type === TokenType.Keyword && this.currentToken.value === keyword;
  }

  private skipOptionalKeyword(keyword: string) {
    if (this.isOptionalKeyword(keyword)) {
      this.nextToken();
    }
  }

  private skipOptionalKeywords(keywords: string[]) {
    if (this.isSeveralOptionalKeywords(keywords)) {
      this.nextToken();
    }
  }

  private isSeveralOptionalKeywords(keyword: string[]) {
    return this.currentToken.type === TokenType.Keyword && keyword.includes(this.currentToken.value);
  }

  private isOptionalTerminator() {
    return this.currentToken.type === TokenType.Terminator;
  }

  private skipOptionalTerminator() {
    if (this.isOptionalTerminator()) {
      this.nextToken();
    }
  }

  private skipMandatoryTerminator() {
    this.expectTerminator();
    this.nextToken();
  }

  private isOptionalIdentifier() {
    return this.currentToken.type === TokenType.Identifier;
  }

  private isOptionalNumeric() {
    return this.currentToken.type === TokenType.NumericLiteral;
  }

  //   // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:comment-entry

  private parseCommentEntry(): string {
    let value: string = '';

    if (this.expectIdentifier()) {
      value = value.concat(this.currentToken.value);
      this.nextToken();
      while (this.currentToken.type === TokenType.Identifier) {
        value = value.concat(' ', this.currentToken.value);
        this.nextToken();
      }
      this.skipOptionalTerminator();
    } else {
      this.nextToken();
    }

    return value;
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:numeric
  private isNumeric() {
    return this.currentToken.type === TokenType.NumericLiteral;
  }

  // https://www.cs.vu.nl/grammarware/vs-cobol-ii/#gdef:nonnumeric
  private isNotNumeric() {
    return this.currentToken.type === TokenType.StringLiteral;
  }
}
