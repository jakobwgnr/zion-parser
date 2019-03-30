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
import { Syntax } from './syntax';

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export class Parser {
  private index: number = -1;
  private currentToken: Token = new Token();
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
    node.type = Syntax.CobolSourceProgram;

    this.nextToken();
    node.setHasError(this.expectKeyword('DIVISION', this.currentToken));
    this.nextToken();
    node.setHasError(this.expectTerminator(this.currentToken));
    this.nextToken();
    const programId: Nodes.ProgramId = this.parseProgramId();
    const identificationDivisionContent: Nodes.IdentificationDivisionContent = this.parseIdentificationDivisionContent();
    node.setHasError(this.expectKeyword('ENVIRONMENT', this.currentToken));
    const environmentDivisionContent: Nodes.EnvironmentDivisionContent = this.parseEnvironmentDivisionContent();
    return this.finalizeNode(
      node,
      new Nodes.CobolSourceProgram(
        node.getStandardInfo(),
        programId,
        identificationDivisionContent,
        environmentDivisionContent,
      ),
      this.currentToken,
    );
  }

  private parseIdentificationDivisionContent(): Nodes.IdentificationDivisionContent {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.IdentificationDivisionContent;
    let author: string = '';
    let installation: string = '';
    let dateWritten: string = '';
    let dateCompiled: string = '';
    let security: string = '';

    /* istanbul ignore else */
    if (this.isOptionalKeyword('AUTHOR', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        author = author.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          author = author.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
        this.nextToken();
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('INSTALLATION', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        installation = installation.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          installation = installation.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-WRITTEN', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        dateWritten = dateWritten.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          dateWritten = dateWritten.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-COMPILED', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        dateCompiled = dateCompiled.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          dateCompiled = dateCompiled.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('SECURITY', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        security = security.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          security = security.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    return this.finalizeNode(
      node,
      new Nodes.IdentificationDivisionContent(
        node.getStandardInfo(),
        author,
        installation,
        dateWritten,
        dateCompiled,
        security,
      ),
      this.currentToken,
    );
  }

  // TODO test missing
  private parseEnvironmentDivisionContent(): Nodes.EnvironmentDivisionContent {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.EnvironmentDivisionContent;
    let configurationSection: Nodes.ConfigurationSection | null = null;
    let inputOutputSection: Nodes.InputOutputSection | null = null;

    this.nextToken();
    node.setHasError(this.expectKeyword('DIVISION', this.currentToken));
    this.nextToken();
    node.setHasError(this.expectTerminator(this.currentToken));
    this.nextToken();

    if (this.isOptionalKeyword('CONFIGURATION', this.currentToken)) {
      configurationSection = this.parseConfigurationSection();
    }

    if (this.isOptionalKeyword('INPUT-OUTPUT', this.currentToken)) {
      inputOutputSection = this.parseInputOutputSection();
    }

    return this.finalizeNode(
      node,
      new Nodes.EnvironmentDivisionContent(node.getStandardInfo(), configurationSection, inputOutputSection),
      this.currentToken,
    );
  }

  // TODO: impl + test missing
  private parseConfigurationSection(): Nodes.ConfigurationSection | null {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.ConfigurationSection;
    let sourceComputerParagraph: Nodes.SourceComputerParagraph | null = null;
    let objectComputerParagraph: Nodes.ObjectComputerParagraph | null = null;
    let specialNamesParagraph: Nodes.SpecialNamesParagraph | null = null;

    this.nextToken();
    node.setHasError(!this.expectKeyword('SECTION', this.currentToken));
    this.nextToken();
    node.setHasError(!this.expectTerminator(this.currentToken));
    this.nextToken();

    if (this.isOptionalKeyword('SOURCE-COMPUTER', this.currentToken)) {
      sourceComputerParagraph = this.parseSourceComputerParagraph();
    }

    if (this.isOptionalKeyword('OBJECT-COMPUTER', this.currentToken)) {
      objectComputerParagraph = this.parseObjectComputerParagraph();
    }

    if (this.isOptionalKeyword('SPECIAL-NAMES', this.currentToken)) {
      specialNamesParagraph = this.parseSpecialNamesParagraph();
    }

    return this.finalizeNode(
      node,
      new Nodes.ConfigurationSection(
        node.getStandardInfo(),
        sourceComputerParagraph,
        objectComputerParagraph,
        specialNamesParagraph,
      ),
      this.currentToken,
    );
  }

  private parseSourceComputerParagraph(): Nodes.SourceComputerParagraph {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.SourceComputerParagraph;
    let sourceComputerValue: string = '';

    this.nextToken();
    node.setHasError(this.expectTerminator(this.currentToken));
    this.nextToken();

    if (this.isOptionalIdentifier(this.currentToken)) {
      sourceComputerValue = this.currentToken.value;
      this.nextToken();
      if (this.isOptionalKeyword('WITH', this.currentToken)) {
        this.nextToken();
        node.setHasError(this.expectKeyword('DEBUGGING', this.currentToken));
        this.nextToken();
        node.setHasError(this.expectKeyword('MODE', this.currentToken));
        this.nextToken();
      } else {
        if (this.isOptionalKeyword('DEBUGGING', this.currentToken)) {
          this.nextToken();
          node.setHasError(this.expectKeyword('MODE', this.currentToken));
          this.nextToken();
        }
      }
      node.setHasError(this.expectTerminator(this.currentToken));
      this.nextToken();
    }
    return this.finalizeNode(
      node,
      new Nodes.SourceComputerParagraph(node.getStandardInfo(), sourceComputerValue),
      this.currentToken,
    );
  }

  private parseObjectComputerParagraph(): Nodes.ObjectComputerParagraph {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.ObjectComputerParagraph;
    let objectComputerValue: string = '';
    let memorySizeValue: string = '';
    let sequenceValue: string = '';
    let segmentLimitValue: string = '';

    this.nextToken();
    node.setHasError(this.expectTerminator(this.currentToken));
    this.nextToken();

    if (this.isOptionalIdentifier(this.currentToken)) {
      objectComputerValue = this.currentToken.value;
      this.nextToken();
      if (this.isOptionalKeyword('MEMORY', this.currentToken)) {
        this.nextToken();
        if (this.isOptionalKeyword('SIZE', this.currentToken)) {
          this.nextToken();
        }
        if (this.expectNumeric(this.currentToken)) {
          memorySizeValue = this.currentToken.value;
        } else {
          node.setHasError(true);
        }
        this.nextToken();
        node.setHasError(!this.expectSeveralKeywords(['WORDS', 'CHARACTERS', 'MODULES'], this.currentToken));
        this.nextToken();

        if (this.isOptionalKeyword('PROGRAM', this.currentToken)) {
          this.nextToken();
          if (this.isOptionalKeyword('COLLATING', this.currentToken)) {
            this.nextToken();
          }
          node.setHasError(this.expectKeyword('SEQUENCE', this.currentToken));
          this.nextToken();
          if (this.isOptionalKeyword('IS', this.currentToken)) {
            this.nextToken();
          }
          if (this.expectIdentifier(this.currentToken)) {
            sequenceValue = this.currentToken.value;
          } else {
            node.setHasError(true);
          }
          this.nextToken();
        }

        if (this.isOptionalKeyword('COLLATING', this.currentToken)) {
          this.nextToken();
          node.setHasError(this.expectKeyword('SEQUENCE', this.currentToken));
          this.nextToken();
          if (this.isOptionalKeyword('IS', this.currentToken)) {
            this.nextToken();
          }
          if (this.expectIdentifier(this.currentToken)) {
            sequenceValue = this.currentToken.value;
          } else {
            node.setHasError(true);
          }
          this.nextToken();
        }

        if (this.isOptionalKeyword('SEQUENCE', this.currentToken)) {
          this.nextToken();
          if (this.isOptionalKeyword('IS', this.currentToken)) {
            this.nextToken();
          }
          if (this.expectIdentifier(this.currentToken)) {
            sequenceValue = this.currentToken.value;
          } else {
            node.setHasError(true);
          }
          this.nextToken();
        }

        if (this.isOptionalKeyword('SEGMENT-LIMIT', this.currentToken)) {
          this.nextToken();
          if (this.isOptionalKeyword('IS', this.currentToken)) {
            this.nextToken();
          }
          if (this.expectIdentifier(this.currentToken)) {
            segmentLimitValue = this.currentToken.value;
          } else {
            node.setHasError(true);
          }
          this.nextToken();
        }
      }
      node.setHasError(!this.expectTerminator(this.currentToken));
      this.nextToken();
    }

    return this.finalizeNode(
      node,
      new Nodes.ObjectComputerParagraph(
        node.getStandardInfo(),
        objectComputerValue,
        memorySizeValue,
        sequenceValue,
        segmentLimitValue,
      ),
      this.currentToken,
    );
  }

  private parseSpecialNamesParagraph(): Nodes.SpecialNamesParagraph {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.SpecialNamesParagraph;
    const specialNamesParagraphStatusPhrases: Nodes.SpecialNamesParagraphStatusPhrase[] = [];
    const specialNamesParagraphClauses: Nodes.SpecialNamesParagraphClause[] = [];
    let currencySignClause: Nodes.CurrencySignClause | null = null;
    let decimalPointClause: Nodes.DecimalPointClause | null = null;

    this.nextToken();
    node.setHasError(this.expectTerminator(this.currentToken));
    this.nextToken();

    while (
      this.isOptionalIdentifier(this.currentToken) ||
      this.isOptionalKeyword('ON', this.currentToken) ||
      this.isOptionalKeyword('OFF', this.currentToken)
    ) {
      specialNamesParagraphStatusPhrases.push(this.parseSpecialNamesStatusPhrase());
    }

    while (
      this.isOptionalKeyword('ALPHABET', this.currentToken) ||
      this.isOptionalKeyword('SYMBOLIC', this.currentToken) ||
      this.isOptionalKeyword('CLASS', this.currentToken) ||
      this.isOptionalKeyword('CURRENCY', this.currentToken) ||
      this.isOptionalKeyword('DECIMAL-POINT', this.currentToken)
    ) {
      if (this.isOptionalKeyword('CURRENCY', this.currentToken)) {
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

      if (this.isOptionalKeyword('DECIMAL-POINT', this.currentToken)) {
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

      if (this.isOptionalKeyword('ALPHABET', this.currentToken)) {
        specialNamesParagraphClauses.push(this.parseAlphabetClause());
      }

      if (this.isOptionalKeyword('SYMBOLIC', this.currentToken)) {
        specialNamesParagraphClauses.push(this.parseSymbolicCharactersClause());
      }

      if (this.isOptionalKeyword('CLASS', this.currentToken)) {
        specialNamesParagraphClauses.push(this.parseClassClause());
      }
    }

    return this.finalizeNode(
      node,
      new Nodes.SpecialNamesParagraph(
        node.getStandardInfo(),
        specialNamesParagraphStatusPhrases,
        specialNamesParagraphClauses,
        currencySignClause,
        decimalPointClause,
      ),
      this.currentToken,
    );
  }

  private parseSpecialNamesStatusPhrase(): Nodes.SpecialNamesParagraphStatusPhrase {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.SpecialNamesParagraphStatusPhrase;
    let environment: string = '';
    let mnemonic: string = '';
    let onCondition: Nodes.Condition | null = null;
    let offCondition: Nodes.Condition | null = null;

    if (this.isOptionalIdentifier(this.currentToken)) {
      environment = this.currentToken.value;
      this.nextToken();
      if (this.isOptionalKeyword('IS', this.currentToken)) {
        this.nextToken();
      }
      mnemonic = this.currentToken.value;
      this.nextToken();
    }

    if (this.isOptionalKeyword('ON', this.currentToken)) {
      this.nextToken();
      if (this.isOptionalKeyword('STATUS', this.currentToken)) {
        this.nextToken();
      }
      if (this.isOptionalKeyword('IS', this.currentToken)) {
        this.nextToken();
      }
      onCondition = this.parseCondition();

      if (this.isOptionalKeyword('OFF', this.currentToken)) {
        this.nextToken();
        if (this.isOptionalKeyword('STATUS', this.currentToken)) {
          this.nextToken();
        }
        if (this.isOptionalKeyword('IS', this.currentToken)) {
          this.nextToken();
        }
        offCondition = this.parseCondition();
      }
    }

    if (this.isOptionalKeyword('OFF', this.currentToken)) {
      this.nextToken();
      if (this.isOptionalKeyword('STATUS', this.currentToken)) {
        this.nextToken();
      }
      if (this.isOptionalKeyword('IS', this.currentToken)) {
        this.nextToken();
      }
      offCondition = this.parseCondition();

      if (this.isOptionalKeyword('ON', this.currentToken)) {
        this.nextToken();
        if (this.isOptionalKeyword('STATUS', this.currentToken)) {
          this.nextToken();
        }
        if (this.isOptionalKeyword('IS', this.currentToken)) {
          this.nextToken();
        }
        onCondition = this.parseCondition();
      }
    }

    return this.finalizeNode(
      node,
      new Nodes.SpecialNamesParagraphStatusPhrase(
        node.getStandardInfo(),
        environment,
        mnemonic,
        onCondition,
        offCondition,
      ),
      this.currentToken,
    );
  }

  private parseCurrencySignClause(): Nodes.CurrencySignClause {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.CurrencySignClause;

    this.nextToken();
    if (this.isOptionalKeyword('SIGN', this.currentToken)) {
      this.nextToken();
    }
    if (this.isOptionalKeyword('IS', this.currentToken)) {
      this.nextToken();
    }
    const currencySignValue = this.currentToken.value;
    this.nextToken();

    return this.finalizeNode(
      node,
      new Nodes.CurrencySignClause(node.getStandardInfo(), currencySignValue),
      this.currentToken,
    );
  }

  private parseSymbolicCharactersClause(): Nodes.SymbolicCharactersClause {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.SymbolicCharacter;
    const symbolicCharacters: string[] = [];
    const symbolicIntegers: string[] = [];
    let inOrdinalPosition: string = '';

    this.nextToken();
    if (this.isOptionalKeyword('CHARACTERS', this.currentToken)) {
      this.nextToken();
    }
    while (this.isOptionalIdentifier(this.currentToken)) {
      if (this.expectIdentifier(this.currentToken)) {
        while (this.isOptionalIdentifier(this.currentToken)) {
          symbolicCharacters.push(this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
        this.nextToken();
      }
      if (this.isSeveralOptionalKeywords(['ARE', 'IS'], this.currentToken)) {
        this.nextToken();
      }

      if (this.expectNumeric(this.currentToken)) {
        while (this.isOptionalNumeric(this.currentToken)) {
          symbolicIntegers.push(this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
        this.nextToken();
      }
    }

    if (this.isOptionalKeyword('IN', this.currentToken)) {
      this.nextToken();
      if (this.expectIdentifier(this.currentToken)) {
        inOrdinalPosition = this.currentToken.value;
      } else {
        node.setHasError(true);
      }
      this.nextToken();
    }

    return this.finalizeNode(
      node,
      new Nodes.SymbolicCharactersClause(
        node.getStandardInfo(),
        symbolicCharacters,
        symbolicIntegers,
        inOrdinalPosition,
      ),
      this.currentToken,
    );
  }

  private parseDecimalPointClause(): Nodes.DecimalPointClause {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.DecimalPointClause;

    this.nextToken();
    if (this.isOptionalKeyword('IS', this.currentToken)) {
      this.nextToken();
    }
    node.setHasError(!this.expectKeyword('COMMA', this.currentToken));
    this.nextToken();

    return this.finalizeNode(node, new Nodes.DecimalPointClause(node.getStandardInfo()), this.currentToken);
  }

  private parseAlphabetClause(): Nodes.AlphabetClause {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.AlphabetClause;
    let alphabetName: string = '';
    let alphabetType: string = '';
    const alphabetLiterals: Nodes.Literal[] = [];

    this.nextToken();
    if (this.expectIdentifier(this.currentToken)) {
      alphabetName = this.currentToken.value;
    } else {
      node.setHasError(true);
    }
    this.nextToken();

    if (this.isOptionalKeyword('IS', this.currentToken)) {
      this.nextToken();
    }

    if (this.isSeveralOptionalKeywords(['STANDARD-1', 'STANDARD-2', 'NATIVE', 'EBCDIC'], this.currentToken)) {
      alphabetType = this.currentToken.value;
      this.nextToken();
    } else {
      if (this.expectIdentifier(this.currentToken)) {
        while (this.isOptionalIdentifier(this.currentToken)) {
          const alphabetLiteral: Nodes.Literal = new Nodes.Literal();
          alphabetLiteral.literal = this.currentToken.value;
          this.nextToken();
          if (this.isSeveralOptionalKeywords(['THROUGH', 'THRU'], this.currentToken)) {
            this.nextToken();
            if (this.expectIdentifier(this.currentToken)) {
              alphabetLiteral.throughLiteral = this.currentToken.value;
            } else {
              node.setHasError(true);
            }
            this.nextToken();
          }

          if (this.isOptionalKeyword('ALSO', this.currentToken)) {
            this.nextToken();
            if (this.expectIdentifier(this.currentToken)) {
              alphabetLiteral.alsoLiteral = this.currentToken.value;
            } else {
              node.setHasError(true);
            }
            this.nextToken();
          }
          alphabetLiterals.push(alphabetLiteral);
        }
      } else {
        node.setHasError(true);
        this.nextToken();
      }
    }

    return this.finalizeNode(
      node,
      new Nodes.AlphabetClause(node.getStandardInfo(), alphabetName, alphabetType, alphabetLiterals),
      this.currentToken,
    );
  }

  private parseClassClause(): Nodes.ClassClause {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.ClassClause;
    let className: string = '';
    const classLiterals: Nodes.Literal[] = [];

    this.nextToken();
    if (this.expectIdentifier(this.currentToken)) {
      className = this.currentToken.value;
    } else {
      node.setHasError(true);
    }
    this.nextToken();

    if (this.isOptionalKeyword('IS', this.currentToken)) {
      this.nextToken();
    }

    while (this.isOptionalIdentifier(this.currentToken)) {
      const classLiteral: Nodes.Literal = new Nodes.Literal();
      classLiteral.literal = this.currentToken.value;
      this.nextToken();
      if (this.isSeveralOptionalKeywords(['THROUGH', 'THRU'], this.currentToken)) {
        this.nextToken();
        if (this.expectIdentifier(this.currentToken)) {
          classLiteral.throughLiteral = this.currentToken.value;
        } else {
          node.setHasError(true);
        }
        this.nextToken();
      }

      classLiterals.push(classLiteral);
    }

    return this.finalizeNode(
      node,
      new Nodes.ClassClause(node.getStandardInfo(), className, classLiterals),
      this.currentToken,
    );
  }

  // TODO ALL
  private parseCondition(): null {
    this.nextToken();
    return null;
  }

  // TODO Impl + test missing
  private parseInputOutputSection(): Nodes.InputOutputSection {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.InputOutputSection;

    this.nextToken();
    node.setHasError(this.expectKeyword('SECTION', this.currentToken));

    return this.finalizeNode(node, new Nodes.InputOutputSection(node.getStandardInfo()), this.currentToken);
  }

  private parseRecordingModeClause(): Nodes.RecordingModeClause {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.RecordingModeClause;
    this.nextToken();
    /* istanbul ignore else */
    if (this.isOptionalKeyword('MODE', this.currentToken)) {
      this.nextToken();
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('IS', this.currentToken)) {
      this.nextToken();
    }

    node.setHasError(!this.expectModeIdentifier(this.currentToken));
    this.nextToken();

    return this.finalizeNode(node, new Nodes.RecordingModeClause(node.getStandardInfo()), this.currentToken);
  }

  private parseProgramId(): Nodes.ProgramId {
    const startNode = this.startNode(this.currentToken);
    startNode.type = Syntax.ProgramId;
    let initialRequired: boolean = false;
    let initialExists: boolean = false;
    let programIdValue: string = '';

    this.nextToken();
    /* istanbul ignore else */
    if (this.isOptionalTerminator(this.currentToken)) {
      this.nextToken();
    }

    if (this.expectIdentifier(this.currentToken)) {
      programIdValue = this.currentToken.value;
    } else {
      startNode.setHasError(true);
    }
    this.nextToken();

    /* istanbul ignore else */
    if (this.isOptionalKeyword('IS', this.currentToken)) {
      initialRequired = true;
      this.nextToken();
    }

    /* istanbul ignore else */
    if (initialRequired) {
      if (this.expectKeyword('INITIAL', this.currentToken)) {
        initialExists = true;
      } else {
        startNode.setHasError(true);
      }
      this.nextToken();
    } else {
      if (this.isOptionalKeyword('INITIAL', this.currentToken)) {
        initialExists = true;
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('PROGRAM', this.currentToken)) {
      if (!initialExists) {
        this.errorHandler.unexpectedTokenError(
          this.currentToken,
          'Keyword "PROGRAM" in the ProgramID statement only allowed if "INITIAL" Keyword is present',
        );
      }
      this.nextToken();
    }

    /* istanbul ignore else */
    if (this.isOptionalTerminator(this.currentToken)) {
      this.nextToken();
    }

    return this.finalizeNode(
      startNode,
      new Nodes.ProgramId(startNode.getStandardInfo(), programIdValue),
      this.currentToken,
    );
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

  private startNode(token: Token) {
    const node = new Node({
      startColumnTotal: token.startColumnTotal,
      startColumnRelative: token.startColumnRelative,
      startLine: token.startLine,
    });
    return node;
  }

  private finalizeNode(startNode: Node, node: any, endToken: Token) {
    node.endColumnRelative = endToken.endColumnRelative;
    node.endColumnTotal = endToken.endColumnTotal;
    node.endLine = endToken.endLine;
    node.type = startNode.type;
    node.hasError = startNode.hasError;

    return node;
  }

  private expectKeyword(keyword: string, token: Token): boolean {
    if (token.type !== TokenType.Keyword || token.value !== keyword) {
      this.errorHandler.unexpectedTokenError(
        token,
        'Expected certain type of keyword. Got different',
        new Token(keyword, TokenType.Keyword),
      );
      return false;
    }
    return true;
  }

  private expectSeveralKeywords(keywords: string[], token: Token): boolean {
    if (token.type !== TokenType.Keyword || !keywords.includes(token.value)) {
      this.errorHandler.unexpectedTokenError(
        token,
        'Expected one of the keywords in the list. Got different',
        new Token(keywords.toString(), TokenType.Keyword),
      );
      return false;
    }
    return true;
  }

  private expectModeIdentifier(token: Token): boolean {
    if (
      token.type !== TokenType.Identifier ||
      !(token.value === 'F' || token.value === 'V' || token.value === 'U' || token.value === 'S')
    ) {
      this.errorHandler.unexpectedTokenError(
        token,
        'Expected special type of Identifier',
        new Token('F or V or U or S', 'ModeIdentifier'),
      );
      return false;
    }
    return true;
  }

  private expectIdentifier(token: Token): boolean {
    if (token.type !== TokenType.Identifier) {
      this.errorHandler.unexpectedTokenError(
        token,
        'Expected identifier token - Got different',
        new Token(undefined, TokenType.Identifier),
      );
      return false;
    }
    return true;
  }

  private expectNumeric(token: Token): boolean {
    if (token.type !== TokenType.NumericLiteral) {
      this.errorHandler.unexpectedTokenError(
        token,
        'Expected numeric token - Got different',
        new Token(undefined, TokenType.NumericLiteral),
      );
      return false;
    }
    return true;
  }

  private expectTerminator(token: Token) {
    if (token.type !== TokenType.Terminator) {
      this.errorHandler.unexpectedTokenError(
        token,
        'Expected terminator token - Got different',
        new Token(undefined, TokenType.Terminator),
      );
      return false;
    }
    return true;
  }

  private isOptionalKeyword(keyword: string, token: Token) {
    return token.type === TokenType.Keyword && token.value === keyword;
  }

  private isSeveralOptionalKeywords(keyword: string[], token: Token) {
    return token.type === TokenType.Keyword && keyword.includes(token.value);
  }

  private isOptionalTerminator(token: Token) {
    return token.type === TokenType.Terminator;
  }

  private isOptionalIdentifier(token: Token) {
    return token.type === TokenType.Identifier;
  }

  private isOptionalNumeric(token: Token) {
    return token.type === TokenType.NumericLiteral;
  }
}
