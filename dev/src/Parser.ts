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
          this.nodeList.push(node);
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

      if (this.expectIdentifier()) {
        author = author.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          author = author.concat(' ', this.currentToken.value);
          this.nextToken();
        }
        this.skipOptionalTerminator();
      } else {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('INSTALLATION')) {
      this.nextToken();

      this.skipOptionalTerminator();

      if (this.expectIdentifier()) {
        installation = installation.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          installation = installation.concat(' ', this.currentToken.value);
          this.nextToken();
        }
        this.skipOptionalTerminator();
      } else {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-WRITTEN')) {
      this.nextToken();

      this.skipOptionalTerminator();

      if (this.expectIdentifier()) {
        dateWritten = dateWritten.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          dateWritten = dateWritten.concat(' ', this.currentToken.value);
          this.nextToken();
        }

        this.skipOptionalTerminator();
      } else {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-COMPILED')) {
      this.nextToken();

      this.skipOptionalTerminator();

      if (this.expectIdentifier()) {
        dateCompiled = dateCompiled.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          dateCompiled = dateCompiled.concat(' ', this.currentToken.value);
          this.nextToken();
        }

        this.skipOptionalTerminator();
      } else {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('SECURITY')) {
      this.nextToken();

      this.skipOptionalTerminator();

      if (this.expectIdentifier()) {
        security = security.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          security = security.concat(' ', this.currentToken.value);
          this.nextToken();
        }

        this.skipOptionalTerminator();
      } else {
        this.nextToken();
      }
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
    const alphabetLiterals: Nodes.Literal[] = [];

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
          const alphabetLiteral: Nodes.Literal = new Nodes.Literal();
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
    const classLiterals: Nodes.Literal[] = [];

    this.nextToken();
    if (this.expectIdentifier()) {
      className = this.currentToken.value;
    }
    this.nextToken();

    this.skipOptionalKeyword('IS');

    while (this.isOptionalIdentifier()) {
      const classLiteral: Nodes.Literal = new Nodes.Literal();
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

    this.expectTerminator();
    this.nextToken();

    while (this.isOptionalKeyword('SELECT')) {
      fileControlEntries.push(this.parseFileControlEntry());
    }

    return this.finalizeNode(new Nodes.FileControlParagraph(startNodeInfo), this.currentToken);
  }

  // TODO Impl + test missing
  private parseFileControlEntry(): Nodes.FileControlEntry {
    const startNodeInfo = this.startNode(this.currentToken);

    this.nextToken();
    // const selectClause = this.parseSelectClause();

    return this.finalizeNode(new Nodes.FileControlEntry(startNodeInfo), this.currentToken);
  }

  private parseSelectClause(): Nodes.SelectClause | null {
    const startNodeInfo = this.startNode(this.currentToken);
    let fileName: string = '';

    this.nextToken();

    this.skipOptionalKeyword('OPTIONAL');

    fileName = this.currentToken.value;
    this.nextToken();

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
    let value: string | Nodes.SpecialRegister = '';
    const dataNames: string[] = [];
    this.nextToken();

    this.skipOptionalKeyword('CHARACTER');
    this.skipOptionalKeyword('IS');

    if (this.isSpecialRegisterKeyword()) {
      value = this.parseSpecialRegister();
    } else {
      if (this.expectIdentifier()) {
        value = this.currentToken.value;
      }
      this.nextToken();

      while (this.isSeveralOptionalKeywords(['IN', 'OF'])) {
        this.skipOptionalKeywords(['IN', 'OF']);
        dataNames.push(this.currentToken.value);
        this.nextToken();
      }
    }

    return this.finalizeNode(new Nodes.PaddingCharacterClause(startNodeInfo, value, dataNames), this.currentToken);
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

  private isSpecialRegisterKeyword(): boolean {
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

  private isOptionalIdentifier() {
    return this.currentToken.type === TokenType.Identifier;
  }

  private isOptionalNumeric() {
    return this.currentToken.type === TokenType.NumericLiteral;
  }
}
