{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Compiler.EntryPoint (entryPoint) where

import Coal.Kernel.LLVM (IRConstruct (CDefine), IRLine (LInstruction), i32)

entryPoint :: IRConstruct [IRLine]
entryPoint = CDefine "main" i32 Nothing [] instructions
 where
  instructions =
    [ LInstruction ["call void @rt_runtime_init()"]
    , LInstruction ["call void @\"Main.main\"(i8* null)"]
    , LInstruction ["ret i32 0"]
    ]
