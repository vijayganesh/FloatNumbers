package org.vricsa.LSTM
package emitVerlog

import chisel3._
import circt.stage.ChiselStage
import org.vricsa.floats.ieeeHPAdd
import org.vricsa.floats.ieeeHPMul
object GenerateMVM extends App{
  ChiselStage.emitSystemVerilogFile(new ieeeHPAdd( 8, 23), Array(
    "--target-dir","RTL/MVM/",
  ), firtoolOpts = Array(
    "--verilog",
    "--disable-all-randomization",
    "--lowering-options=disallowLocalVariables,disallowPackedArrays",
    "-O=release",
  )
  )
}

// object GenerateFir extends App{
//   ChiselStage.emitFIRRTLDialect(new LSTMTop(4,8,7), Array(
//     "--target-dir","FIR/B16/4/",
//   )

//   )
// }

// object GenerateFloatingAdd extends  App{
//   ChiselStage.emitSystemVerilogFile(new ieeeHPAdd(8,23),Array(
//     // Specify target directory
//     "--target-dir","RTL/ieee",

//   ), firtoolOpts = Array(
//     "--verilog",

//     "--disable-all-randomization",
//     "--lowering-options=disallowLocalVariables",

//     "-O=release",


//   )
//   )
// }
// object GenerateVerilog extends App{

// ChiselStage.emitSystemVerilogFile(new MACpf(5,9),Array(

//    // Specify target directory
//   "--target-dir","RTL",

//   ), firtoolOpts = Array(
//   "--verilog",
//  //"--split-verilog",
//   //"--ir-fir",
//   "--disable-all-randomization",
//   "--lowering-options=disallowLocalVariables",
//   //"--no-comments",
//     "-O=release",
//   //"-o","RTL/Split/", // This is for split output
// )
// )
// }

// //print(GenerateVerilog.emitVerilog(new Foo()))
// object GenerateMVM extends App{

//   ChiselStage.emitSystemVerilogFile(new Seq1(2,8,23), Array(

//     // Specify target directory
//     "--target-dir","RTL",


//   ), firtoolOpts = Array(
//     "--verilog",
//     //"--split-verilog",
//     //"--ir-fir",

//     "--disable-all-randomization",
//     "--lowering-options=disallowLocalVariables,disallowPackedArrays",
//     //"--no-comments",
//     "-O=release",
//     //"-o","RTL/Split/", // This is for split output
//   )
//   )
// }

// object GenerateSeq2 extends App{

//   ChiselStage.emitSystemVerilogFile(new Seq2(8,23), Array(
//     // Specify target directory
//     "--target-dir","RTL",
//   ), firtoolOpts = Array(
//     "--verilog",
//     //"--split-verilog",
//     //"--ir-fir",
//     "--disable-all-randomization",
//     "--lowering-options=disallowLocalVariables,disallowPackedArrays",
//     //"--no-comments",
//     "-O=release",
//     //"-o","RTL/Split/", // This is for split output
//   )
//   )
// }

// object GenerateLstmTop extends App{
//   var parallel = 8
//   val mantissa = 7
//   val exponent = 8
//   // Bfloat 7,8
//   // f16 5,10
//   // f32 8,23

//   ChiselStage.emitSystemVerilogFile(new LSTMTop(parallel,exponent,mantissa), Array(
//     // Specify target directory
//     "--target-dir","RTL/b16/"+parallel+"/",
//   ), firtoolOpts = Array(
//     "--verilog",
//     //"--split-verilog",
//     //"--ir-fir",
//     "--disable-all-randomization",
//     "--lowering-options=disallowLocalVariables,disallowPackedArrays",
//     //"--no-comments",
//     "-O=release",
//     //"-o","RTL/Split/", // This is for split output
//   )
//   )
// }