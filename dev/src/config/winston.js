var appRoot = require('app-root-path');
var winston = require('winston');

// define the custom settings for each transport (file, console)
var options = {
  console: {
    colorize: true,
    handleExceptions: true,
    json: false,
    level: 'debug',
  },
  file: {
    colorize: false,
    filename: `${appRoot}/logs/app.log`,
    handleExceptions: true,
    json: true,
    level: 'info',
    maxFiles: 5,
    maxsize: 5242880, // 5MB
  },
};

// instantiate a new Winston Logger with the settings defined above
var logger = new winston.createLogger({
  exitOnError: false, // do not exit on handled exceptions
  transports: [new winston.transports.File(options.file), new winston.transports.Console(options.console)],
});

// create a stream object with a 'write' function that will be used by `morgan`
logger.stream = {
  write: function(message, encoding) {
    // use the 'info' log level so the output will be picked up by both transports (file and console)
    logger.info(message);
  },
};

module.exports = logger;
