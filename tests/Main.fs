module FablePlus.Tests.Main

open Fable.Core.JsInterop

// This is necessary to make webpack collect all test files
importSideEffects "./FablePlus.test.fs"
