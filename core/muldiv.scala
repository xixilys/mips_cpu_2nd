
package mips_cpu_2nd

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class unsigned_div extends BlackBox{

    val io = IO(new Bundle {
        val aclk = Input(Clock())
        val s_axis_divisor_tvalid  = Input(UInt(1.W))
        val s_axis_divisor_tready  = Output(UInt(1.W))
        val s_axis_divisor_tdata   = Input(UInt(32.W))
        val s_axis_dividend_tvalid = Input(UInt(1.W))
        val s_axis_dividend_tready = Output(UInt(1.W))
        val s_axis_dividend_tdata  = Input(UInt(32.W))
        val m_axis_dout_tvalid     = Output(UInt(1.W))
        val m_axis_dout_tdata      = Output(UInt(64.W))
  })

}

class signed_div extends BlackBox{

    val io = IO(new Bundle {
        val aclk = Input(Clock())
        val s_axis_divisor_tvalid  = Input(UInt(1.W))
        val s_axis_divisor_tready  = Output(UInt(1.W))
        val s_axis_divisor_tdata   = Input(UInt(32.W))
        val s_axis_dividend_tvalid = Input(UInt(1.W))
        val s_axis_dividend_tready = Output(UInt(1.W))
        val s_axis_dividend_tdata  = Input(UInt(32.W))
        val m_axis_dout_tvalid     = Output(UInt(1.W))
        val m_axis_dout_tdata      = Output(UInt(64.W))
  })
}

class muldiv extends Module with mips_macros{ //觉得除法器那一块有很多可以改的东西，但是怕改了出问题，还是不要改了吧

    val io = IO(new Bundle { //有隐式的时钟与复位，并且复位为高电平复位
        //流水线中的延迟器
        val en  = Input(UInt(1.W))
        val ctrl = Input(UInt(32.W))
       
        val in1       = Input(UInt(32.W))
        val in2       = Input(UInt(32.W))

        val hi_in = Input(UInt(32.W))
        val lo_in = Input(UInt(32.W))

        val hi        = Output(UInt(32.W))
        val lo        = Output(UInt(32.W))
        val reg_write = Output(Bool())
        val reg_out   = Output(UInt(32.W))
        val pending   = Output(UInt(1.W))
    })
    val cat_value = Cat(io.hi_in,io.lo_in)
    val counter_Reg = RegInit(0.U(1.W))
    val last_counter_Reg = RegInit(0.U(1.W))
    last_counter_Reg := counter_Reg

    val madd_answer = Wire(UInt(64.W))
    val maddu_answer = Wire(UInt(64.W))
    val msub_answer = Wire(UInt(64.W))
    val msubu_answer = Wire(UInt(64.W))

    val mulu_answer = Wire(UInt(64.W))
    val mul_answer = Wire(UInt(64.W))

    // val hi_r = RegInit(0.U(32.W)) // 由于除法器用的ip核为时序电路不是组合逻辑,为了优化
    // val lo_r = RegInit(0.U(32.W))//用来存储除法器输出的数据

    val in1_valid_u = RegInit(0.U(1.W))
    val in2_valid_u = RegInit(0.U(1.W))
    val in1_valid = RegInit(0.U(1.W))//RegInit(0.U(1.W))
    val in2_valid = RegInit(0.U(1.W))
    val divu_output_valid =  Wire(UInt(1.W))
    val div_output_valid =  Wire(UInt(1.W))

    val in2_ready_u =Wire(UInt(1.W))
    val in1_ready_u =Wire(UInt(1.W))
    val in2_ready =Wire(UInt(1.W))
    val in1_ready =Wire(UInt(1.W))

    val divisor_Reg = Wire(UInt(32.W))
   // val divisor_Reg = RegInit(0.U(32.W))//除数
    val dividend_Reg = Wire(UInt(32.W))
   // val dividend_Reg = RegInit(0.U(32.W))//被除数


    divisor_Reg := Mux(io.en.asBool,io.in2,0.U)//这地方写的我就是沙比，如果真的是寄存器的 话
    dividend_Reg := Mux(io.en.asBool,io.in1,0.U)//这地方写的我就是沙比

    val divu_answer  = Wire(UInt(64.W))
    val div_answer  = Wire(UInt(64.W))

    in1_valid := 1.U
    in2_valid := 1.U
    in1_valid_u := 1.U
    in2_valid_u := 1.U

    
 

    val output_valid_tag = divu_output_valid.asBool || div_output_valid.asBool


    val a = RegInit(0.U(32.W))
    val b = RegInit(0.U(1.W))
    val limit = Wire(UInt(32.W))
    limit := Mux(io.ctrl(0),34.U,32.U)
    a := Mux(counter_Reg.asBool,Mux(a === limit,0.U,(a+1.U)),0.U)
    b := Mux(a === limit,1.U,0.U)

   // io.b := b
    when(io.en.asBool && !counter_Reg.asBool) {
        when(io.ctrl(0) ) {
            when(in1_ready.asBool) {
                counter_Reg := 1.U
            }
        }.elsewhen(io.ctrl(1)){ 
            when(in1_ready_u.asBool) {
                counter_Reg:= 1.U
            }

        }
    }.otherwise{
        when(b.asBool) {
            counter_Reg := 0.U

        }

    }
    io.pending := Mux(io.en.asBool && (io.ctrl(0) || io.ctrl(1) ),Mux(last_counter_Reg === 1.U && counter_Reg === 0.U,0.U,1.U),0.U)


    io.lo := Mux1H(Seq(
        io.ctrl(ALU_DIV) -> div_answer(63,32),
        io.ctrl(ALU_DIVU) -> divu_answer(63,32),
        io.ctrl(ALU_MULT) -> mul_answer(31,0),
        io.ctrl(ALU_MULTU) -> mulu_answer(31,0),

        io.ctrl(ALU_MADD) -> madd_answer(31,0),
        io.ctrl(ALU_MADDU) -> maddu_answer(31,0),
        io.ctrl(ALU_MSUB)   -> msub_answer(31,0),
        io.ctrl(ALU_MSUBU) -> msubu_answer(31,0)

    ))

    io.hi := Mux1H(Seq(
        io.ctrl(ALU_DIV)  -> div_answer(31,0),
        io.ctrl(ALU_DIVU) -> divu_answer(31,0),
        io.ctrl(ALU_MULT)  -> mul_answer(63,32),
        io.ctrl(ALU_MULTU)  -> mulu_answer(63,32),

        io.ctrl(ALU_MADD) -> madd_answer(63,32),
        io.ctrl(ALU_MADDU) -> maddu_answer(63,32),
        io.ctrl(ALU_MSUB)   -> msub_answer(63,32),
        io.ctrl(ALU_MSUBU) -> msubu_answer(63,32)

    ))
    
    io.reg_out := Mux1H(Seq(
        io.ctrl(ALU_MUL) -> mul_answer(31,0)
    ))
    io.reg_write := io.ctrl(ALU_MUL)
    // io.lo 0


    val _udiv = Module(new unsigned_div)  //时钟信号最好保持均为上升沿触发
    _udiv.io.aclk := clock//反向的时钟来驱动触除法器，以保证下一周期需要的时候肯定能有结果，并且输入也可以使用寄存器
    _udiv.io.s_axis_divisor_tvalid :=  in2_valid_u //I2为除数
    _udiv.io.s_axis_divisor_tdata  :=  divisor_Reg
    _udiv.io.s_axis_dividend_tvalid :=  in1_valid_u 
    _udiv.io.s_axis_dividend_tdata :=  dividend_Reg
    in2_ready_u :=  _udiv.io.s_axis_divisor_tready
    in1_ready_u :=  _udiv.io.s_axis_dividend_tready
    divu_answer :=  _udiv.io.m_axis_dout_tdata
    divu_output_valid :=  _udiv.io.m_axis_dout_tvalid


    val _div  = Module(new signed_div)    
    _div.io.aclk := clock
    _div.io.s_axis_divisor_tvalid :=  in2_valid //I2为除数
    _div.io.s_axis_divisor_tdata  :=  divisor_Reg
    _div.io.s_axis_dividend_tvalid :=  in1_valid 
    _div.io.s_axis_dividend_tdata :=  dividend_Reg
    in2_ready :=  _div.io.s_axis_divisor_tready
    in1_ready :=  _div.io.s_axis_dividend_tready
    div_answer :=  _div.io.m_axis_dout_tdata
    div_output_valid :=  _div.io.m_axis_dout_tvalid

    mulu_answer := io.in1 * io.in2
    mul_answer := (io.in1.asSInt * io.in2.asSInt).asUInt

    madd_answer := (cat_value.asSInt + mul_answer.asSInt).asUInt
    maddu_answer := cat_value + mul_answer
    msub_answer := (cat_value.asSInt - mul_answer.asSInt).asUInt
    msubu_answer := cat_value - mul_answer

  
}

// object muldiv_test extends App{
//     (new ChiselStage).emitVerilog(new muldiv )
// }

