module IR.Specs.Lexicals.Identifier where

data IIdentifier = Identifier {unIdentifier :: String} | ReservedId {unReservedId :: String} deriving (Eq, Ord)

instance Show IIdentifier where
  show (Identifier i) = "(Identifier \"" ++ i ++ "\")"
  show (ReservedId i) = "(ReservedId \"" ++ i ++ "\")"

getId :: IIdentifier -> String
getId (Identifier i) = i
getId (ReservedId i) = i

reservedKeywords :: [String]
reservedKeywords =
  [ "contract",
    "constructor",
    "public",
    "static",
    "if",
    "else",
    "require",
    "exit",
    "true",
    "false",
    "bool",
    "int",
    "bytes",
    "loop",
    "function",
    "return",
    "new",
    "import",
    -- "this", -- `this` can be used without literal change
    "auto",
    "struct",
    "library",
    "const",
    "type",
    "state",
    "PrivKey",
    "PubKey",
    "Ripemd160",
    "Sha1",
    "Sha256",
    "Sig",
    "SigHashType",
    "OpCodeType",
    "SigHashPreimage",
    "asm"
  ]
    ++ reservedNames

reservedNames :: [String]
reservedNames =
  [ libTx,
    libUtils,
    libSigHash,
    funcHash160,
    funcHash256,
    funcCheckSig,
    funcPropagateState,
    funcCheckInitBalance,
    varTxPreimage,
    varOutput,
    varOutputScript,
    varMsgSender,
    varMsgValue,
    varSig,
    varPubKey,
    varReturned,
    varRetVal,
    varLoopBreakFlag,
    varLoopCount,
    varContractBalance,
    varInitBalance
  ]

libTx :: String
libTx = "Tx"

libUtils :: String
libUtils = "Utils"

libSigHash :: String
libSigHash = "SigHash"

funcHash160 :: String
funcHash160 = "hash160"

funcHash256 :: String
funcHash256 = "hash256"

funcCheckSig :: String
funcCheckSig = "checkSig"

funcPropagateState :: String
funcPropagateState = "propagateState"

funcCheckInitBalance :: String
funcCheckInitBalance = "checkInitBalance"

funcValue :: String
funcValue = "value"

funcIsFirstCall :: String
funcIsFirstCall = "isFirstCall"

varTxPreimage :: String
varTxPreimage = "txPreimage"

varOutput :: String
varOutput = "output"

varOutputScript :: String
varOutputScript = "outputScript"

varMsgSender :: String
varMsgSender = "msgSender"

varMsgValue :: String
varMsgValue = "msgValue"

varSig :: String
varSig = "sig"

varPubKey :: String
varPubKey = "pubKey"

varReturned :: String
varReturned = "returned"

varRetVal :: String
varRetVal = "ret"

varLoopBreakFlag :: String
varLoopBreakFlag = "loopBreakFlag"

varLoopCount :: String
varLoopCount = "__LoopCount__"

varLoopContinueFlag :: String
varLoopContinueFlag = "loopContinueFlag"

varContractBalance :: String
varContractBalance = "contractBalance"

varInitBalance :: String
varInitBalance = "initBalance"
