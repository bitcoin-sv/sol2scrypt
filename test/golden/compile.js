const glob = require('glob');
const { join } = require('path');
const { compileContract, findCompiler, compilerVersion } = require('scryptlib');

function compileAllContracts() {


    const scryptc = findCompiler();
    console.log('compiler binary: ', scryptc)
    console.log('compiler version: ', compilerVersion(scryptc))
    let nSucceeded = 0;
    let nFailed = 0;
    const contracts = glob.sync(join(join(__dirname, "pass"), './*.scrypt'));
    contracts.forEach(filePath => {

        const result = compileContract(filePath);

        if (result.errors.length > 0) {
            nFailed++;
            console.log(`Contract ${filePath} compiling failed with errors:`);
        } else {
            nSucceeded++;
            console.log(`Contract ${filePath} compiling succeeded.`);
        }
    })

    console.log(`compiling finish. ${nSucceeded} succeeds and ${nFailed} fails`);
}


compileAllContracts();
