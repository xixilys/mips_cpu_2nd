package mips_cpu_2nd

import chisel3._
import chisel3.stage._
import chisel3.util._
import org.yaml.snakeyaml.events.Event.ID
class decoder_port extends Bundle {
        // val   BranchD_Flag  =   Output(UInt(1.W))
        // val   RegWriteD     =   Output(UInt(1.W))
        // val   MemToRegD     =   Output(UInt(1.W))
        // val   MemWriteD     =   Output(UInt(1.W))
       
        // val   ALUSrcD       =   Output(UInt(2.W))
        // val   RegDstD=      Output(UInt(2.W))
        // val   ImmUnsigned=      Output(UInt(1.W))
        // val   BranchD=      Output(UInt(6.W))
        // val   JumpD=      Output(UInt(1.W))
        // val   JRD=      Output(UInt(1.W))
        // val   LinkD=      Output(UInt(1.W))
        // val   HiLoWriteD=      Output(UInt(2.W))
        // val   HiLoToRegD=      Output(UInt(2.W))
        // val   CP0WriteD=      Output(UInt(1.W))
        // val   CP0ToRegD=      Output(UInt(1.W))
        // val   LoadUnsignedD=      Output(UInt(1.W))
        // val   MemWidthD=      Output(UInt(2.W))
        // val   MemRLD=      Output(UInt(2.W))
        // val   BadInstrD=      Output(UInt(1.W))
        // val   BreakD=      Output(UInt(1.W))
        // val   SysCallD=      Output(UInt(1.W))
        // val   EretD   =   Output(UInt(1.W))
        //src1 -> rs src2 ->rt dest -> rd 默认大部分指令是这样的，但是有一部分不是的就得单独的分析
        //===================这些还没写完，但是已经懒得写了==============================
        val src1_has = Output(Bool())
        val src2_has = Output(Bool())
        val dest_has = Output(Bool())
        //===================   ==========  =========================    =====

        val src1 = Output(UInt(5.W))
        val src2 = Output(UInt(5.W))
        val dest = Output(UInt(5.W))
        
        val ALUCtrlD      =   Output(UInt(32.W)) // 独热码
        val inst_opcode = Vec(7,Output(UInt(6.W)))
        val inst_type   = Vec(7,Output(Bool()))
        //   ALU MULDIV MEM branch PRIVILEGE self_in(自献) 还有一类就是伪nop指令
        val imm_value = Output(UInt()) 
        val BadInstrD=      Output(UInt(1.W))
}



class cu extends Module with mips_macros {
    val io1 = IO(new Bundle{
        val   InstrD = Input(UInt(32.W))
       
        // val   BreakD=      Output(UInt(1.W))
        // val   SysCallD=      Output(UInt(1.W))
        // val   EretD   =   Output(UInt(1.W))
    })   
    val io = IO(new decoder_port )

    val ImmUnsigned = Wire(Bool()) //为0表示立即数需要进行无符号扩展
    val OpD = io1.InstrD(31,26)//首字母为o的大写(>_<)
    val FunctD = io1.InstrD(5,0)
    val coD = io1.InstrD(25)
    val coD_res = io1.InstrD(24,21)
    val RsD    = io1.InstrD(25,21)
    val RtD    = io1.InstrD(20,16)
    val RdD    = io1.InstrD(15,11)
    val i_type = Wire(Bool())//表明该指令用到了立即数
    i_type     := MuxLookup(OpD,0.U,Seq(
        ( OP_ADDI) -> 1.U,
        ( OP_ANDI) -> 1.U,
        ( OP_ADDIU)-> 1.U,
        ( OP_SLTI) -> 1.U,
        ( OP_SLTIU)-> 1.U,
        ( OP_LUI)  -> 1.U,
        ( OP_ORI)  -> 1.U,
        ( OP_XORI) -> 1.U,
        ( OP_BEQ)  -> 1.U,
        ( OP_BNE ) -> 1.U,
        ( OP_BGTZ) -> 1.U,
        ( OP_BLEZ) -> 1.U,
        ( OP_J   ) -> 1.U,
        ( OP_JAL)  -> 1.U,
        ( OP_LB)   -> 1.U,
        ( OP_LBU)  -> 1.U,
        ( OP_LH)   -> 1.U,
        ( OP_LHU ) -> 1.U,
        ( OP_LW )  -> 1.U,
        ( OP_SB)   -> 1.U,
        ( OP_SH)   -> 1.U,
        ( OP_SW)   -> 1.U,
        (OP_LWL )  -> 1.U,
        (OP_LWR )  -> 1.U,
        (OP_SWL )  -> 1.U,
        (OP_SWR )  -> 1.U,
        OP_REGIMM  -> MuxLookup(RtD,0.U,Seq( //后面这里可以改,在id时就开始算分支
            RT_TEQI     -> 1.U,
            RT_TNEI     -> 1.U,
            RT_TGEI     -> 1.U,
            RT_TGEIU    -> 1.U,
            RT_TLTI     -> 1.U,
            RT_TLTIU    -> 1.U
        ))))
    val ImmD      = Mux(ImmUnsigned, unsign_extend(io1.InstrD(15,0),16),sign_extend(io1.InstrD(15,0),16))
    io.imm_value := ImmD
//==============================不对
    io.src1 := RsD//Mux(RsD)
    io.src2 := RtD
    io.src2_has := 1.U
    io.src1_has := !i_type.asBool
    io.dest_has := 1.U
    io.dest := Mux(i_type.asBool,RsD,RdD)
//==============================不想写了===================

        
//==============================
    ImmUnsigned := MuxLookup(OpD,0.U,Seq(
         OP_ADDIU   -> 1.U,
         OP_SLTIU   -> 1.U,
         OP_LBU     -> 1.U,
         OP_LHU     -> 1.U,
         OP_SPECIAL -> MuxLookup(FunctD,0.U,Seq(
            FUNC_SLTU  ->  1.U,
            FUNC_SUBU  ->  1.U,
            FUNC_SUBU  ->  1.U,
            FUNC_ADDU  ->  1.U,
            FUNC_DIVU  ->  1.U,
            FUNC_MULTU ->  1.U,
            FUNC_TGEU  ->  1.U,
            FUNC_MADDU ->  1.U,
            FUNC_MSUBU ->  1.U)),
        OP_REGIMM -> MuxLookup(RtD,0.U,Seq( //后面这里可以改,在id时就开始算分支
            RT_TGEIU     -> (ID_TGEIU.U ),
            RT_TLTIU    -> (ID_TLTIU.U )
        ))))
    
    val alu_type = MuxLookup(OpD,0.U,Seq(
        OP_ADDI    -> 1.U,
        OP_ANDI    -> 1.U,
        OP_ADDIU   -> 1.U,
        OP_SLTI    -> 1.U,
        OP_SLTIU   -> 1.U,
        OP_LUI     -> 1.U,
        OP_ORI     -> 1.U,
        OP_XORI    -> 1.U,
        OP_SPECIAL -> MuxLookup(FunctD,0.U,Seq( // 在op相同情况下，根据funct来判断是哪一条指令 这个得写特判
            FUNC_SUB  ->  1.U,
            FUNC_AND  ->  1.U,
            FUNC_OR   ->  1.U,
            FUNC_SLT  ->  1.U,
            FUNC_SLL  ->  1.U,
            FUNC_SLTU ->  1.U,
            FUNC_XOR ->  1.U,
            FUNC_ADD  ->  1.U,
            FUNC_ADDU ->  1.U,
            FUNC_SUBU ->  1.U,
            FUNC_NOR  ->  1.U,
            FUNC_SLLV ->  1.U,
            FUNC_SRA  ->  1.U,
            FUNC_SRAV ->  1.U,
            FUNC_SRL  ->  1.U,
            FUNC_SRLV ->  1.U,
            FUNC_MOVN ->  1.U,
            FUNC_MOVZ ->  1.U)),
       
        OP_SPECIAL2 -> MuxLookup(FunctD,0.U,Seq(
            FUNC_CLO  -> 1.U,
            FUNC_CLZ  -> 1.U
    ))))
    io.inst_opcode(opcode_alu) := MuxLookup(OpD,ID_NULL.U,Seq(
        OP_ADDI    -> ID_ADDI.U,
        OP_ANDI    -> ID_ANDI.U,
        OP_ADDIU   -> ID_ADDIU.U,
        OP_SLTI    -> ID_SLTI.U,
        OP_SLTIU   -> ID_SLTIU.U,
        OP_LUI     -> ID_LUI.U,
        OP_ORI     -> ID_ORI.U,
        OP_XORI    -> ID_XORI.U,
        OP_SPECIAL -> MuxLookup(FunctD,ID_NULL.U,Seq( // 在op相同情况下，根据funct来判断是哪一条指令 这个得写特判
            FUNC_SUB  ->  ID_SUB.U,
            FUNC_AND  ->  ID_AND.U,
            FUNC_OR   ->  ID_OR.U,
            FUNC_SLT  ->  ID_SLT.U,
            FUNC_SLL  ->  ID_SLL.U,
            FUNC_SLTU ->  ID_SLTU.U,
            FUNC_XOR ->   ID_XOR.U,
            FUNC_ADD  ->  ID_ADD.U,
            FUNC_ADDU ->  ID_ADDU.U,
            FUNC_SUBU ->  ID_SUBU.U,
            FUNC_NOR  ->  ID_NOR.U,
            FUNC_SLLV ->  ID_SLLV.U,
            FUNC_SRA  ->  ID_SRA.U,
            FUNC_SRAV ->  ID_SRAV.U,
            FUNC_SRL  ->  ID_SRL.U,
            FUNC_SRLV ->  ID_SRLV.U,
            FUNC_MOVN ->  ID_MOVN.U,
            FUNC_MOVZ ->  ID_MOVZ.U)),
       
        OP_SPECIAL2 -> MuxLookup(FunctD,ID_NULL.U,Seq(
            FUNC_CLO  -> ID_CLO.U,
            FUNC_CLZ  -> ID_CLZ.U
    ))))

    val branch_type = MuxLookup(OpD,0.U,Seq(  
        OP_BEQ   -> 1.U,
        OP_BNE   -> 1.U,
        OP_BGTZ  -> 1.U,
        OP_BLEZ  -> 1.U,
        OP_J     -> 1.U,
        OP_JAL   -> 1.U,
        OP_REGIMM -> MuxLookup(RtD,0.U,Seq( //后面这里可以改,在id时就开始算分支
            RT_BGEZ    -> 1.U,
            RT_BGEZAL  -> 1.U,
            RT_BLTZ    -> 1.U,
            RT_BLTZAL  -> 1.U
        ))))
    io.inst_opcode(opcode_branch) := MuxLookup(OpD,0.U,Seq(  
        ( OP_BEQ) -> (ID_BEQ).U,
        ( OP_BNE ) -> (ID_BNE.U ),
        ( OP_BGTZ) -> (ID_BGTZ.U),
        ( OP_BLEZ) -> (ID_BLEZ).U,
        ( OP_J   ) -> (ID_J.U ),
        ( OP_JAL) -> (ID_JAL.U),
       ( OP_SPECIAL) -> MuxLookup(FunctD,ID_NULL.U,Seq( 
            ( FUNC_JR)   ->   (ID_JR).U,
            ( FUNC_JALR) ->   (ID_JALR.U)
       ))))

    val muldiv_type = MuxLookup(OpD,0.U,Seq(
        OP_SPECIAL -> MuxLookup(FunctD,0.U,Seq( // 在op相同情况下，根据funct来判断是哪一条指令 这个得写特判
            FUNC_DIV   ->   1.U,
            FUNC_DIVU  ->   1.U,
            FUNC_MULT  ->   1.U,
            FUNC_MULTU ->   1.U)),
        OP_SPECIAL2 -> MuxLookup(FunctD,0.U,Seq(
            FUNC_MUL    -> 1.U,
            FUNC_MADD   -> 1.U,
            FUNC_MADDU -> 1.U,
            FUNC_MSUB -> 1.U,
            FUNC_MSUBU -> 1.U
        ))))
    io.inst_opcode(opcode_muldiv) := MuxLookup(OpD,ID_NULL.U,Seq(
        OP_SPECIAL -> MuxLookup(FunctD,ID_NULL.U,Seq( // 在op相同情况下，根据funct来判断是哪一条指令 这个得写特判
            ( FUNC_DIV) ->   (ID_DIV).U,
            ( FUNC_DIVU) ->   (ID_DIVU).U,
            ( FUNC_MULT) ->   (ID_MULT).U,
            ( FUNC_MULTU) ->   (ID_MULTU).U)),
        OP_SPECIAL2 -> MuxLookup(FunctD,ID_NULL.U,Seq(
            FUNC_MUL    -> ID_MUL.U,
            FUNC_MADD   -> ID_MADD.U,
            FUNC_MADDU -> ID_MADDU.U,
            FUNC_MSUB -> ID_MSUB.U,
            FUNC_MSUBU -> ID_MSUBU.U
        ))))
    
    val mem_type = MuxLookup(FunctD,0.U,Seq(
        OP_LBU  -> 1.U,
        OP_LH   -> 1.U,
        OP_LHU  -> 1.U,
        OP_LW   -> 1.U,
        OP_SB   -> 1.U,
        OP_SH   -> 1.U,
        OP_SW   -> 1.U,  
        OP_LWL  -> 1.U,
        OP_LWR  -> 1.U,
        OP_SWL  -> 1.U,
        OP_SWR  -> 1.U
    ))
    io.inst_opcode(opcode_mem) := MuxLookup(FunctD,ID_NULL.U,Seq(
        OP_LB   -> (ID_LB.U),
        OP_LBU  -> (ID_LBU.U),
        OP_LH   -> (ID_LH.U),
        OP_LH   -> (ID_LHU.U),
        OP_LW   -> (ID_LW.U),
        OP_SB   -> (ID_SB.U),
        OP_SH   -> (ID_SH.U),
        OP_SW   -> (ID_SW.U),
        OP_LWL  -> (ID_LWL).U,
        OP_LWR  -> (ID_LWR).U,
        OP_SWL  -> (ID_SWL).U,
        OP_SWR  -> (ID_SWR).U,
    ))
    val privilege_type = MuxLookup(FunctD,0.U,Seq(
        OP_CACHE  -> 1.U, 
        OP_SPECIAL  -> MuxLookup(FunctD,0.U,Seq( 
            FUNC_BREAK   ->   1.U,
            FUNC_SYSCALL ->   1.U)),
        OP_PRIVILEGE -> MuxLookup(coD,0.U,Seq(
            CO_SET -> MuxLookup(FunctD,1.U,Seq(
                    FUNC_TLBP  -> 1.U,
                    FUNC_TLBR  -> 1.U,
                    FUNC_TLBWI -> 1.U,
                    FUNC_TLBWR -> 1.U,
                    FUNC_ERET  -> 1.U,
                    FUNC_WAIT  -> 1.U
            )),
            CO_RESET -> MuxLookup(coD_res,0.U,Seq(
                    COP_MFC0 -> 1.U,
                    COP_MTC0 -> 1.U
            ))
    ))))
    io.inst_opcode(opcode_privilege) := MuxLookup(FunctD,ID_NULL.U,Seq(
        OP_CACHE    -> ID_CACHE.U, 
        OP_SPECIAL  -> MuxLookup(FunctD,0.U,Seq( 
            FUNC_BREAK   ->   ID_BREAK.U,
            FUNC_SYSCALL ->   ID_SYSCALL.U)),
        OP_PRIVILEGE -> MuxLookup(coD,ID_NULL.U,Seq(
            CO_SET -> MuxLookup(FunctD,ID_NULL.U,Seq(
                    FUNC_TLBP  -> ID_TLBP.U,
                    FUNC_TLBR  -> ID_TLBR.U,
                    FUNC_TLBWI -> ID_TLBWI.U,
                    FUNC_TLBWR -> ID_TLBWR.U,
                    FUNC_ERET  -> ID_ERET.U,
                    FUNC_WAIT  -> ID_WAIT.U
            )),
            CO_RESET -> MuxLookup(coD_res,ID_NULL.U,Seq(
                    COP_MFC0 -> ID_MFC0.U,
                    COP_MTC0 -> ID_MTC0.U
            ))
    ))))
    val nop_type = MuxLookup(OpD,0.U,Seq(
         OP_PREF   -> 1.U
    ))
    io.inst_opcode(opcode_nop) := MuxLookup(OpD,ID_NULL.U,Seq(
         OP_PREF   -> ID_PREF.U
    ))
    val self_in_type = MuxLookup(OpD,0.U,Seq(
        OP_SPECIAL -> MuxLookup(FunctD,0.U,Seq( 
            FUNC_TEQ      -> 1.U,
            FUNC_TNE      -> 1.U,
            FUNC_TGEU     -> 1.U,
            FUNC_TGE      -> 1.U,
            FUNC_TLT      -> 1.U,
            FUNC_TLTU     -> 1.U)),
        OP_REGIMM -> MuxLookup(RtD,0.U,Seq( //后面这里可以改,在id时就开始算分支
            RT_TEQI     -> 1.U,
            RT_TNEI     -> 1.U,
            RT_TGEI     -> 1.U,
            RT_TGEIU    -> 1.U,
            RT_TLTI     -> 1.U,           
            RT_TLTIU    -> 1.U ))))
    io.inst_opcode(opcode_self_in) := MuxLookup(OpD,0.U,Seq(
        (OP_SPECIAL) -> MuxLookup(FunctD,ID_NULL.U,Seq( // 在op相同情况下，根据funct来判断是哪一条指令 这个得写特判
            FUNC_TEQ      -> ID_TEQ.U,
            FUNC_TNE      -> ID_TNE.U,
            FUNC_TGEU     -> ID_TGEU.U,
            FUNC_TGE      -> ID_TGE.U,
            FUNC_TLT      -> ID_TLT.U,
            FUNC_TLTU     -> ID_TLTU.U)),
        OP_REGIMM -> MuxLookup(RtD,ID_NULL.U,Seq( //后面这里可以改,在id时就开始算分支 
            RT_TEQI       -> ID_TEQI.U ,
            RT_TNEI       -> ID_TNEI.U ,
            RT_TGEI       -> ID_TGEI.U ,
            RT_TGEIU      -> ID_TGEIU.U,
            RT_TLTI       -> ID_TLTI.U ,
            RT_TLTIU      -> ID_TLTIU.U))))

    io.inst_type(opcode_alu) := alu_type
    io.inst_type(opcode_muldiv) := muldiv_type
    io.inst_type(opcode_mem) := mem_type
    io.inst_type(opcode_self_in) := self_in_type
    io.inst_type(opcode_nop) := nop_type
    io.inst_type(opcode_branch) := branch_type
    io.inst_type(opcode_privilege) := privilege_type
    // io.BranchD_Flag := Mux(OpD === )


    io.BadInstrD := Cat(io.inst_type(opcode_alu),io.inst_type(opcode_branch),io.inst_type(opcode_mem),io.inst_type(opcode_muldiv),
        io.inst_type(opcode_nop),io.inst_type(opcode_privilege),io.inst_type(opcode_self_in))  === 0.U   
    val alu_type_opcode = io.inst_opcode(alu_type)
    val get_alu_op = Wire(UInt(32.W))
    io.ALUCtrlD  := Mux(reset.asBool,1.U<<0,get_alu_op)
    get_alu_op := MuxLookup(alu_type_opcode,1.U<<ALU_NULL,Seq(
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

        (ID_MUL     ).U -> (1.U<<ALU_MUL) ,
        (ID_CLO     ).U ->(1.U<<ALU_CLO) ,
        (ID_CLZ     ).U ->(1.U<<ALU_CLZ) ,
        (ID_MOVN    ).U -> (1.U<<ALU_MOVN) ,
        (ID_MOVZ    ).U ->(1.U<<ALU_MOVZ) ,
        (ID_MADD    ).U ->(1.U<<ALU_MADD) ,
        (ID_MADDU   ).U -> (1.U<<ALU_MADDU) ,
        (ID_MSUB    ).U ->(1.U<<ALU_MSUB) ,
        (ID_MSUBU   ).U ->(1.U<<ALU_MSUBU) ,    
    ))
    get_alu_op := Cat(alu_type_opcode === ID_NULL.U,
         0.U(1.W),
        alu_type_opcode === ID_ADD.U || alu_type_opcode === ID_ADDI.U,
        alu_type_opcode === ID_ADDU.U || alu_type_opcode === ID_ADDIU.U,
        alu_type_opcode === ID_AND.U  || alu_type_opcode === ID_ANDI.U,
        0.U(1.W), // alu_type_opcode === ID_DIV.U , 
        0.U(1.W),//alu_type_opcode === ID_DIVU.U  ,
        alu_type_opcode === ID_LUI.U , 
        0.U(2.W),//alu_type_opcode === ID_MULT.U ,
        //alu_type_opcode === ID_MULTU.U  ,
        alu_type_opcode === ID_NOR.U , 
        alu_type_opcode === ID_OR.U ,
        alu_type_opcode === ID_SLL.U  ||   alu_type_opcode === ID_SLLV.U,
        alu_type_opcode === ID_SLT.U  ||   alu_type_opcode === ID_SLTI.U,
        alu_type_opcode === ID_SLTU.U ||   alu_type_opcode === ID_SLTIU.U,
        alu_type_opcode === ID_SRA.U  ||   alu_type_opcode === ID_SRAV.U,
        alu_type_opcode === ID_SRL.U ||   alu_type_opcode === ID_SRLV.U,
        1.U(1.W),
        alu_type_opcode === ID_SUB.U  ,//SUBE
        alu_type_opcode === ID_SUBU.U , //SUBU
        alu_type_opcode === ID_XOR.U || alu_type_opcode === ID_XORI.U,
        alu_type_opcode === ID_MOVN.U ,
        alu_type_opcode === ID_MOVZ.U  ,
        alu_type_opcode === ID_CLO.U , 
        alu_type_opcode === ID_CLZ.U ,
        0.U(5.W)
        // alu_type_opcode === ID_MUL.U ,
        // alu_type_opcode === ID_MADD.U ,
        // alu_type_opcode === ID_MADDU.U ,        
        // alu_type_opcode === ID_MSUB.U , 
        // alu_type_opcode === ID_MSUBU.U 
        )
}


object cu_test extends App{
    (new ChiselStage).emitVerilog(new cu)
}


