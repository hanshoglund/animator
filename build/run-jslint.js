
/*jslint
    node: true, 
    evil: true, 
    stupid: true, 
    vars: true
*/
/*globals
    require, JSLINT
*/

var fs = require('fs');
var clc = require('cli-color');
eval(String(fs.readFileSync('lib/jslint/jslint.js')));

var style = {
    error  : clc.red,
    file   : clc.cyan,
    notice : clc.green,
    warn   : clc.yellow
};

function printUsage() {
    console.log('Usage: node run-jslint.js  path');
    console.log('');
}

function printError(error) {
    console.log(style.error('Error: ') + 'Could not open file: ' + error);
}

(function () {
    var options = {
        sloppy  : true,
        white   : true
    };
    var path = process.argv[2];
    var source = '';
    var result;

    if (!path) {
        printUsage();
        process.exit(1);
    }
    
    try {
        source = fs.readFileSync(path, 'utf8');        
    } catch (error) {
        printError(path);
        process.exit(1);
    }

    console.log(style.notice('JSLint:') + ' ' + path);
    result = JSLINT(source, options);

    if (result) {
        console.log(style.notice('    PASSED'));
        process.exit(0);
    } else {
        console.log();
        JSLINT.errors.map(function(error) {
            if (error) {
                // console.log(Object.keys(error));
                console.log(String() + style.file(path) + ' ' + error.line + ':' + error.character);
                console.log('  ' + style.error(error.reason));
                if (error.evidence) {
                    console.log('  ' + error.evidence.slice(0,200));
                }
                console.log();
            }
        });        
        process.exit(1);
    }    
}());

