const glob = require('glob');
const { join, basename } = require('path');
const { exit } = require('process');
const { compileContract, findCompiler, compilerVersion } = require('scryptlib');

function compileAllContracts() {

    const exclude = ["erc721.scrypt", "vote.scrypt"]
    const scryptc = findCompiler();
    console.log('compiler binary: ', scryptc)
    console.log('compiler version: ', compilerVersion(scryptc))
    let nSucceeded = 0;
    let nFailed = 0;
    const contracts = glob.sync(join(join(__dirname, "pass"), './*.scrypt'));
    contracts.forEach(filePath => {

        if(exclude.includes(basename(filePath))) {
            console.log(`ignore Contract ${filePath}`);
            return;
        }

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

    if(nFailed > 0) {
        exit(-1);
    }
}


compileAllContracts();
