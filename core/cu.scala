package mips_cpu_2nd

import chisel3._
import chisel3.stage._
import chisel3.util._
import org.yaml.snakeyaml.events.Event.ID
class decoder_port extends Bundle {
        val   BranchD_Flag  =   Output(UInt(1.W))
        val   RegWriteD     =   Output(UInt(1.W))
        val   MemToRegD     =   Output(UInt(1.W))
        val   MemWriteD     =   Output(UInt(1.W))
        val   ALUCtrlD      =   Output(UInt(24.W)) // 独热码
        val   ALUSrcD       =   Output(UInt(2.W))
        val   RegDstD=      Output(UInt(2.W))
        val   ImmUnsigned=      Output(UInt(1.W))
        val   BranchD=      Output(UInt(6.W))
        val   JumpD=      Output(UInt(1.W))
        val   JRD=      Output(UInt(1.W))
        val   LinkD=      Output(UInt(1.W))
        val   HiLoWriteD=      Output(UInt(2.W))
        val   HiLoToRegD=      Output(UInt(2.W))
        val   CP0WriteD=      Output(UInt(1.W))
        val   CP0ToRegD=      Output(UInt(1.W))
        val   LoadUnsignedD=      Output(UInt(1.W))
        val   MemWidthD=      Output(UInt(2.W))
        val   MemRLD=      Output(UInt(2.W))
        // val   BadInstrD=      Output(UInt(1.W))
        // val   BreakD=      Output(UInt(1.W))
        // val   SysCallD=      Output(UInt(1.W))
        // val   EretD   =   Output(UInt(1.W))
}



class cu extends Module with mips_macros {
    val io1 = IO(new Bundle{
        val   InstrD = Input(UInt(32.W))
        val   BadInstrD=      Output(UInt(1.W))
        val   BreakD=      Output(UInt(1.W))
        val   SysCallD=      Output(UInt(1.W))
        val   EretD   =   Output(UInt(1.W))
    })   
    val io = IO(new decoder_port )

    
    val OpD = io1.InstrD(31,26)//首字母为o的大写(>_<)
    val FunctD = io1.InstrD(5,0)
    val coD = io1.InstrD(25)
    val coD_res = io1.InstrD(24,21)
    val RsD    = io1.InstrD(25,21)
    val RtD    = io1.InstrD(20,16)
    val ins_id = Wire(UInt(7.W))


    // io.BranchD_Flag := Mux(OpD === )
    ins_id := MuxLookup(OpD,ID_NULL.U,Seq(
        ( OP_ADDI) -> (ID_ADDI).U,
        ( OP_ANDI) -> (ID_ANDI).U,
        ( OP_ADDIU) -> (ID_ADDIU).U,
        ( OP_SLTI) -> (ID_SLTI).U,
        ( OP_SLTIU) -> (ID_SLTIU).U,
        ( OP_LUI) -> (ID_LUI).U,
        ( OP_ORI) -> (ID_ORI).U,
        ( OP_XORI) -> (ID_XORI).U,
        ( OP_BEQ) -> (ID_BEQ).U,
        ( OP_BNE ) -> (ID_BNE.U ),
        ( OP_BGTZ) -> (ID_BGTZ.U),
        ( OP_BLEZ) -> (ID_BLEZ).U,
        ( OP_J   ) -> (ID_J.U ),
        ( OP_JAL) -> (ID_JAL.U),
        ( OP_LB) -> (ID_LB.U),
        ( OP_LBU) -> (ID_LBU.U),
        ( OP_LH) -> (ID_LH.U),
        ( OP_LHU ) -> (ID_LHU.U ),
        ( OP_LW ) ->(ID_LW.U),
        ( OP_SB) -> (ID_SB.U),
        ( OP_SH) -> (ID_SH.U),
        ( OP_SW) -> (ID_SW.U),
        (OP_LWL ) -> (ID_LWL).U,
        (OP_LWR ) -> (ID_LWR).U,
        (OP_SWL ) -> (ID_SWL).U,
        (OP_SWR ) -> (ID_SWR).U,
        OP_CACHE  -> (ID_CACHE).U, //cache指令，后面应该是只实现了其中几个，后续仔细讨论
        OP_PREF   -> (ID_PREF).U,
        ( OP_SPECIAL) -> MuxLookup(FunctD,ID_NULL.U,Seq( // 在op相同情况下，根据funct来判断是哪一条指令 这个得写特判
            ( FUNC_SUB) ->   (ID_SUB).U,
            ( FUNC_AND ) ->   (ID_AND ).U,
            ( FUNC_OR) ->   (ID_OR).U,
            ( FUNC_SLT) ->   (ID_SLT).U,
            ( FUNC_SLL) ->   (ID_SLL).U,
            ( FUNC_SLTU ) ->   (ID_SLTU ).U,
            ( FUNC_XOR) ->   (ID_XOR).U,
            ( FUNC_ADD) ->   (ID_ADD).U,
            ( FUNC_ADDU ) ->   (ID_ADDU ).U,
            ( FUNC_SUBU ) ->   (ID_SUBU ).U,
            ( FUNC_DIV) ->   (ID_DIV).U,
            ( FUNC_DIVU) ->   (ID_DIVU).U,
            ( FUNC_MULT) ->   (ID_MULT).U,
            ( FUNC_MULTU) ->   (ID_MULTU).U,
            ( FUNC_NOR) ->   (ID_NOR).U,
            ( FUNC_SLLV ) ->   (ID_SLLV ).U,
            ( FUNC_SRA) ->   (ID_SRA).U,
            ( FUNC_SRAV) ->   (ID_SRAV).U,
            ( FUNC_SRL) ->   (ID_SRL).U,
            ( FUNC_SRLV) ->   (ID_SRLV ).U,
            ( FUNC_JR) ->   (ID_JR).U,
            ( FUNC_JALR) ->   (ID_JALR.U),
            ( FUNC_MFHI ) ->   (ID_MFHI.U ),
            ( FUNC_MFLO) ->   (ID_MFLO.U ),
            ( FUNC_MTHI) ->   (ID_MTHI.U),
            ( FUNC_MTLO) ->   (ID_MTLO.U),
            ( FUNC_BREAK ) ->   (ID_BREAK.U ),
            ( FUNC_SYSCALL ) ->   (ID_SYSCALL.U ),
            FUNC_MOVN       -> (ID_MOVN.U),
            FUNC_MOVZ       -> ID_MOVZ.U,
            FUNC_TEQ  -> (ID_TEQ.U),
            FUNC_TNE     -> (ID_TNE.U),
            FUNC_TGEU     -> ID_TGEU.U,
            FUNC_TGE    -> ID_TGE.U,
            FUNC_TLT    -> ID_TLT.U,
            FUNC_TLTU -> ID_TLTU.U
            )),
        (  OP_SPECIAL2) -> MuxLookup(FunctD,ID_NULL.U,Seq(
            FUNC_CLO -> ID_CLO.U,
            FUNC_CLZ    -> ID_CLZ.U,
            FUNC_MUL    -> ID_MUL.U,
            FUNC_MADD   -> ID_MADD.U,
            FUNC_MADDU -> ID_MADDU.U,
            FUNC_MSUB -> ID_MSUB.U,
            FUNC_MSUBU -> ID_MSUBU.U
        )),
        ( OP_REGIMM) -> MuxLookup(RtD,ID_NULL.U,Seq( //后面这里可以改,在id时就开始算分支
            ( RT_BGEZ )  -> (ID_BGEZ.U ),
            ( RT_BGEZAL )  -> (ID_BGEZAL.U ),
            ( RT_BLTZ )  -> (ID_BLTZ.U ),
            ( RT_BLTZAL )  -> (ID_BLTZAL.U ),
            RT_TEQI -> (ID_TEQI.U ),
            RT_TNEI     -> (ID_TNEI.U ),
            RT_TGEI     -> (ID_TGEI.U ),
            RT_TGEIU     -> (ID_TGEIU.U ),
            RT_TLTI     -> (ID_TLTI.U),
            RT_TLTIU    -> (ID_TLTIU.U )
        )),
        ( OP_PRIVILEGE) -> MuxLookup(coD,ID_NULL.U,Seq(
            CO_SET -> MuxLookup(FunctD,ID_NULL.U,Seq(
                    FUNC_TLBP -> ID_TLBP.U,
                    FUNC_TLBR -> ID_TLBR.U,
                    FUNC_TLBWI -> ID_TLBWI.U,
                    FUNC_TLBWR  -> ID_TLBWR.U,
                    FUNC_ERET -> ID_ERET.U,
                    FUNC_WAIT -> ID_WAIT.U
            )),
            CO_RESET -> MuxLookup(coD_res,ID_NULL.U,Seq(
                    COP_MFC0 -> ID_MFC0.U,
                    COP_MTC0 -> ID_MTC0.U
            ))

        ))
        // MuxCase(ID_NULL.U,Seq( //后面这里可以改,在id时就开始算分支
        //     (RsD === RS_ERET )  -> (ID_ERET.U ),
        //     (RsD === RS_MFC0 )  -> (ID_MFC0.U ),
        //     (RsD === RS_MTC0 )  -> (ID_MTC0.U )
        // ))

    ))

    io1.BadInstrD := ins_id === (ID_NULL ).U
    io1.BreakD    := ins_id === (ID_BREAK).U
    io1.SysCallD  := ins_id === (ID_SYSCALL).U
    io1.EretD     := ins_id === (ID_ERET).U

    val get_controls = Wire(UInt(29.W))
    io.MemRLD       := get_controls(28,27)
    io.BranchD_Flag := get_controls(26)
    io.RegWriteD := get_controls(25)   //实在不知道咋写呜呜呜
    io.RegDstD   := get_controls(24,23) 
    io.ALUSrcD   := get_controls(22,21)
    io.ImmUnsigned := get_controls(20) 
    io.BranchD   := get_controls(19,14)
    io.JumpD     := get_controls(13)
    io.JRD       := get_controls(12) 
    io.LinkD     := get_controls(11)
    io.HiLoWriteD:= get_controls(10,9)
    io.HiLoToRegD := get_controls(8,7)
    io.CP0WriteD  := get_controls(6)
    io.CP0ToRegD  := get_controls(5)
    io.MemWriteD  := get_controls(4) 
    io.MemToRegD  := get_controls(3)
    io.LoadUnsignedD :=  get_controls(2)
    io.MemWidthD :=get_controls(1,0)   
    get_controls := MuxLookup(ins_id,0.U,Seq(
            (ID_NULL   ).U -> CTRL_NULL,
            (ID_ADDI   ).U -> CTRL_ITYPE,
            (ID_SUB    ).U -> CTRL_RTYPE,
            (ID_AND    ).U -> CTRL_RTYPE,
            (ID_OR     ).U -> CTRL_RTYPE,
            (ID_XOR    ).U -> CTRL_RTYPE,
            (ID_SLT    ).U -> CTRL_RTYPE,
            (ID_SLL    ).U -> CTRL_RTYPES,
            (ID_ANDI   ).U -> CTRL_ITYPEU,
            (ID_ADD    ).U -> CTRL_RTYPE,
            (ID_ADDU   ).U -> CTRL_RTYPE,
            (ID_ADDIU  ).U -> CTRL_ITYPE,
            (ID_SUBU   ).U -> CTRL_RTYPE,
            (ID_SLTI   ).U -> CTRL_ITYPE,
            (ID_SLTU   ).U -> CTRL_RTYPE,
            (ID_SLTIU  ).U -> CTRL_ITYPE,
            (ID_DIV    ).U -> CTRL_DIV,
            (ID_DIVU   ).U -> CTRL_DIV,
            (ID_MULT   ).U -> CTRL_ITYPEU,
            (ID_SLLV   ).U -> CTRL_RTYPE,
            (ID_SRA    ).U -> CTRL_RTYPES,
            (ID_SRAV   ).U -> CTRL_RTYPE,
            (ID_SRL    ).U -> CTRL_RTYPES,
            (ID_SRLV   ).U -> CTRL_RTYPE,
            (ID_BEQ    ).U -> CTRL_BEQ,
            (ID_BNE    ).U -> CTRL_BNE,
            (ID_BGEZ   ).U -> CTRL_BGEZ,
            (ID_BGEZAL ).U -> CTRL_BGEZAL,
            (ID_BGTZ   ).U -> CTRL_BGTZ,
            (ID_BLEZ   ).U -> CTRL_BLEZ,
            (ID_BLTZ   ).U -> CTRL_BLTZ,
            (ID_BLTZAL ).U -> CTRL_BLTZAL,
            (ID_J      ).U -> CTRL_J,
            (ID_JAL    ).U -> CTRL_JAL,
            (ID_JR     ).U -> CTRL_JR,
            (ID_JALR   ).U -> CTRL_JALR,
            (ID_MFHI   ).U -> CTRL_MFHI,
            (ID_MFLO   ).U -> CTRL_MFLO,
            (ID_MTHI   ).U -> CTRL_MTHI,
            (ID_MTLO   ).U -> CTRL_MTLO,
            (ID_BREAK  ).U -> CTRL_BREAK,
            (ID_SYSCALL).U -> CTRL_SYSCALL,
            (ID_LB     ).U -> CTRL_LB,
            (ID_LBU    ).U -> CTRL_LBU,
            (ID_LH     ).U -> CTRL_LH,
            (ID_LHU    ).U -> CTRL_LHU,
            (ID_LW     ).U -> CTRL_LW,
            (ID_SB     ).U -> CTRL_SB,
            (ID_SH     ).U -> CTRL_SH,
            (ID_SW     ).U -> CTRL_SW,
            (ID_ERET   ).U -> CTRL_ERET,
            (ID_MFC0   ).U -> CTRL_MFC0,
            (ID_MTC0   ).U -> CTRL_MTC0,
            (ID_SWL     .U) -> CTRL_SWL,
            (ID_SWR   ) .U -> CTRL_SWR,
            (ID_LWL   ) .U-> CTRL_LWL,
            (ID_LWR   ) .U-> CTRL_LWR
    ))

    val get_alu_op = Wire(UInt(24.W))
    io.ALUCtrlD  := Mux(reset.asBool,1.U<<ALU_NULL,get_alu_op)

    get_alu_op := MuxLookup(ins_id,1.U<<ALU_NULL,Seq(
        (ID_NULL    ).U  ->(1.U<<ALU_NULL) ,
        (ID_ADD     ).U  ->(1.U<<ALU_ADDE) ,
        (ID_ADDI    ).U  ->(1.U<<ALU_ADDE) ,
        (ID_ADDU    ).U  ->(1.U<<ALU_ADDU) ,
        (ID_ADDIU   ).U  ->(1.U<<ALU_ADDU) ,
        (ID_SUB     ).U  ->(1.U<<ALU_SUBE) ,
        (ID_SUBU    ).U  ->(1.U<<ALU_SUBU) ,
        (ID_SLT     ).U  ->(1.U<<ALU_SLT) ,
        (ID_SLTI    ).U  ->(1.U<<ALU_SLT) ,
        (ID_SLTU    ).U  ->(1.U<<ALU_SLTU) ,
        (ID_SLTIU   ).U  ->(1.U<<ALU_SLTU) ,
        (ID_DIV     ).U  ->(1.U<<ALU_DIV) ,
        (ID_DIVU    ).U  ->(1.U<<ALU_DIVU) ,
        (ID_MULT    ).U  ->(1.U<<ALU_MULT) ,
        (ID_MULTU   ).U  ->(1.U<<ALU_MULTU) ,
        (ID_AND     ).U  ->(1.U<<ALU_AND) ,
        (ID_ANDI    ).U  ->(1.U<<ALU_AND) ,
        (ID_LUI     ).U  ->(1.U<<ALU_LUI) ,
        (ID_NOR     ).U  ->(1.U<<ALU_NOR) ,
        (ID_OR      ).U  ->(1.U<<ALU_OR) ,
        (ID_ORI     ).U  ->(1.U<<ALU_OR) ,
        (ID_XOR     ).U  ->(1.U<<ALU_XOR) ,
        (ID_XORI    ).U  ->(1.U<<ALU_XOR) ,
        (ID_SLL     ).U  ->(1.U<<ALU_SLL) ,
        (ID_SLLV    ).U  ->(1.U<<ALU_SLL) ,
        (ID_SRA     ).U  ->(1.U<<ALU_SRA) ,
        (ID_SRAV    ).U  ->(1.U<<ALU_SRA) ,
        (ID_SRL     ).U  ->(1.U<<ALU_SRL) ,
        (ID_SRLV    ).U  ->(1.U<<ALU_SRL) ,

        (ID_MUL).U -> (1.U<<ALU_MUL) ,
        (ID_CLO).U ->(1.U<<ALU_CLO) ,
        (ID_CLZ).U ->(1.U<<ALU_CLZ) ,
        (ID_MOVN).U -> (1.U<<ALU_MOVN) ,
        (ID_MOVZ).U ->(1.U<<ALU_MOVZ) ,
        (ID_MADD).U ->(1.U<<ALU_MADD) ,
        (ID_MADDU).U -> (1.U<<ALU_MADDU) ,
        (ID_MSUB).U ->(1.U<<ALU_MSUB) ,
        (ID_MSUBU).U ->(1.U<<ALU_MSUBU) ,
        // (ID_BEQ     ).U  ->(1.U<<ALU_SUB) ,
        // (ID_BNE     ).U  ->(1.U<<ALU_SUB) ,
        // (ID_BGEZ    ).U  ->(1.U<<ALU_SUB) ,
        // (ID_BGEZAL  ).U  ->(1.U<<ALU_SUB) ,
        // (ID_BGTZ    ).U  ->(1.U<<ALU_SUB) ,
        // (ID_BLEZ    ).U  ->(1.U<<ALU_SUB) ,
        // (ID_BLTZ    ).U  ->(1.U<<ALU_SUB) ,
        // (ID_BLTZAL  ).U  ->(1.U<<ALU_SUB) ,
        // (ID_J       ).U  ->(1.U<<ALU_NULL) ,
        // (ID_JAL     ).U  ->(1.U<<ALU_NULL) ,
        // (ID_JR      ).U  ->(1.U<<ALU_NULL) ,
        // (ID_JALR    ).U  ->(1.U<<ALU_NULL) ,
        // (ID_MFHI    ).U  ->(1.U<<ALU_NULL) ,
        // (ID_MFLO    ).U  ->(1.U<<ALU_NULL) ,
        // (ID_MTHI    ).U  ->(1.U<<ALU_NULL) ,
        // (ID_MTLO    ).U  ->(1.U<<ALU_NULL) ,
        // (ID_BREAK   ).U  ->(1.U<<ALU_NULL) ,
        // (ID_SYSCALL ).U  ->(1.U<<ALU_NULL) ,
        // (ID_LB      ).U  ->(1.U<<ALU_ADD) ,
        // (ID_LBU     ).U  ->(1.U<<ALU_ADD) ,
        // (ID_LH      ).U  ->(1.U<<ALU_ADD) ,
        // (ID_LHU     ).U  ->(1.U<<ALU_ADD) ,
        // (ID_LW      ).U  ->(1.U<<ALU_ADD) ,
        // (ID_SB      ).U  ->(1.U<<ALU_ADD) ,
        // (ID_SH      ).U  ->(1.U<<ALU_ADD) ,
        // (ID_SW      ).U  ->(1.U<<ALU_ADD) ,
        // (ID_SWL     ).U  ->(1.U<<ALU_ADD) ,
        // (ID_SWR     ).U  ->(1.U<<ALU_ADD) ,
        // (ID_LWL     ).U  ->(1.U<<ALU_ADD) ,
        // (ID_LWR     ).U  ->(1.U<<ALU_ADD) ,
        (ID_ERET    ).U  ->(1.U<<ALU_NULL) ,
        (ID_MFC0    ).U  ->(1.U<<ALU_NULL) ,
        (ID_MTC0    ).U  ->(1.U<<ALU_NULL)        
    ))
}


// object cu1_test extends App{
//     (new ChiselStage).emitVerilog(new cu_1)
// }


