{-# LANGUAGE ScopedTypeVariables #-}

module IdentifierSpec where


import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Identifier" $ do
  let itIdentifier sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult (Sol.Identifier SourceRange) IR.IIdentifier' (Maybe (NameAnn Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt
  describe "#Identifier" $ do
    itIdentifier "aaa" "aaa"
    itIdentifier "aZ_$0" "aZ__0"
    itIdentifier "a" "a"
    itIdentifier "Af" "Af"
    itIdentifier "$aa" "_aa"
    itIdentifier "A4535aa$" "A4535aa_"
    itIdentifier "this" "this"
    itIdentifier "$$$this" "___this"

  describe "#keywords" $ do
    itIdentifier "asm" "userDefined_asm"
    itIdentifier "OpCodeType" "userDefined_OpCodeType"
    itIdentifier "SigHashPreimage" "userDefined_SigHashPreimage"
    itIdentifier "SigHashType" "userDefined_SigHashType"
    itIdentifier "Sig" "userDefined_Sig"
    itIdentifier "Sha256" "userDefined_Sha256"
    itIdentifier "Sha1" "userDefined_Sha1"
    itIdentifier "Ripemd160" "userDefined_Ripemd160"
    itIdentifier "PubKey" "userDefined_PubKey"
    itIdentifier "PrivKey" "userDefined_PrivKey"
    itIdentifier "state" "userDefined_state"
    itIdentifier "type" "userDefined_type"
    itIdentifier "const" "userDefined_const"
    itIdentifier "library" "userDefined_library"
    itIdentifier "struct" "userDefined_struct"
    itIdentifier "auto" "userDefined_auto"
    itIdentifier "import" "userDefined_import"
    itIdentifier "new" "userDefined_new"
    itIdentifier "return" "userDefined_return"
    itIdentifier "function" "userDefined_function"
    itIdentifier "loop" "userDefined_loop"
    itIdentifier "bytes" "userDefined_bytes"
    itIdentifier "int" "userDefined_int"
    itIdentifier "bool" "userDefined_bool"
    itIdentifier "exit" "userDefined_exit"
    itIdentifier "contract" "userDefined_contract"
    
  describe "#reservedNames" $ do
    itIdentifier "Tx" "userDefined_Tx"
    itIdentifier "Utils" "userDefined_Utils"
    itIdentifier "SigHash" "userDefined_SigHash"
    itIdentifier "hash160" "userDefined_hash160"
    itIdentifier "hash256" "userDefined_hash256"
    itIdentifier "checkSig" "userDefined_checkSig"
    itIdentifier "txPreimage" "userDefined_txPreimage"
    itIdentifier "output" "userDefined_output"
    itIdentifier "outputScript" "userDefined_outputScript"
    itIdentifier "msgSender" "userDefined_msgSender"
    itIdentifier "msgValue" "userDefined_msgValue"
    itIdentifier "sig" "userDefined_sig"
    itIdentifier "pubKey" "userDefined_pubKey"


    

