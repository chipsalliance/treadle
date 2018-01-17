;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit SignedModule : 
  module SignedModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : SInt<10>, out : SInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = eq(io.in, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 66:24]
    node _T_7 = lt(io.in, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 66:35]
    wire _T_10 : {eq : UInt<1>, lt : UInt<1>} @[Comparison.scala 25:19]
    _T_10 is invalid @[Comparison.scala 25:19]
    _T_10.eq <= _T_5 @[Comparison.scala 26:12]
    _T_10.lt <= _T_7 @[Comparison.scala 27:12]
    wire _T_12 : {zero : UInt<1>, neg : UInt<1>} @[Sign.scala 54:73]
    _T_12 is invalid @[Sign.scala 54:73]
    _T_12.zero <= _T_10.eq @[Sign.scala 59:19]
    _T_12.neg <= _T_10.lt @[Sign.scala 62:19]
    node _T_13 = bits(io.in, 9, 9) @[SIntTypeClass.scala 70:24]
    node _T_15 = sub(asSInt(UInt<1>("h00")), io.in) @[SIntTypeClass.scala 28:50]
    node _T_16 = tail(_T_15, 1) @[SIntTypeClass.scala 28:50]
    node _T_17 = asSInt(_T_16) @[SIntTypeClass.scala 28:50]
    node _T_18 = mux(_T_13, _T_17, io.in) @[SIntTypeClass.scala 140:31]
    node _T_20 = eq(io.in, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 66:24]
    node _T_22 = lt(io.in, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 66:35]
    wire _T_25 : {eq : UInt<1>, lt : UInt<1>} @[Comparison.scala 25:19]
    _T_25 is invalid @[Comparison.scala 25:19]
    _T_25.eq <= _T_20 @[Comparison.scala 26:12]
    _T_25.lt <= _T_22 @[Comparison.scala 27:12]
    wire _T_27 : {zero : UInt<1>, neg : UInt<1>} @[Sign.scala 54:73]
    _T_27 is invalid @[Sign.scala 54:73]
    _T_27.zero <= _T_25.eq @[Sign.scala 59:19]
    _T_27.neg <= _T_25.lt @[Sign.scala 62:19]
    node _T_28 = mux(_T_27.zero, io.in, io.in) @[TypeclassSpec.scala 62:68]
    node _T_29 = mux(_T_12.neg, _T_18, _T_28) @[TypeclassSpec.scala 62:21]
    io.out <= _T_29 @[TypeclassSpec.scala 25:10]
    
