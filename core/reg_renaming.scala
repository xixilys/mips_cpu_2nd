package mips_cpu_2nd

import chisel3._
import chisel3.stage._
import chisel3.util._
import firrtl.PrimOps
import scala.math._
import scala.reflect.runtime.Macros
import javax.swing.plaf.basic.BasicToolBarUI


class Look_up_table_for_free_list(length : Int,width : Int) extends Module  with mips_macros{
//类似于cache，但是不存在替换算法
    val addr_width = (log10(length)/log10(2)).toInt
    val io = IO(new Bundle {
        val ar_addr  = Input(UInt(addr_width.W))
        val aw_addr  = Input(UInt(addr_width.W))
        val write = Input(Bool()) // 0 => 不写入 01 => 部分写入 10 => 全写入
        val in = Input(UInt(width.W))
        val out = Output(UInt(width.W))
    })
    val table = Reg(Vec(length,(UInt(width.W)))) // 这可以算是读优先的关系🕗

    io.out := table(io.aw_addr)
    for(  i <- 0 to length - 1) {//高电平复位🏇   
        table(i) := Mux(reset.asBool,i.asUInt,Mux(io.write && i.asUInt === io.aw_addr,io.in,table(i)))
    }
}

class  free_list (length :Int,width :Int) extends Module  with mips_macros{
    val write_num = 2
    val read_num  = 2
    val bank_width = log2Up(write_num.max(read_num))
    val bank_num  = pow(2,bank_width).toInt
    val length_width = (log10(length)/log10(2)).toInt
    val io = IO(new Bundle { 
        val read_en = Input(UInt(2.W))
        val write_en = Input(UInt(2.W))//0为前面的
        val read_out  = Vec(read_num,Output(UInt(width.W)))//0为前面的
        val write_in  = Vec(write_num,Input(UInt(width.W)))//0为前面的


        val empty = Output(Bool()) //浪费一点空间无所谓，只要剩余的空间小于最大的写入空间，就算满
        // val empty = Output(Bool()) //只有满足超过发射大小的情况下才叫做不空 ,不需要empty判定吧，我这个算是写优先】的效果
        //感觉应该还算比较难满的把 感觉
    })
    val fifo_banks = VecInit(Seq.fill(bank_num)(Module(new Look_up_table_for_free_list(length,width)).io))

    // for()
    val banks_points = RegInit(0.U(1.W))
    val length_points = RegInit(0.U(length_width.W))
    // val read_banks_points = RegInit(0.U(bank_width.W))
    // val read_length_points = RegInit(0.U(length_width.W))
    for(i <- 0 until bank_num) {
        fifo_banks(i.asUInt).aw_addr := MuxLookup(i.asUInt,0.U,Seq(
            banks_points -> length_points,
            (banks_points + 1.U) -> Mux(banks_points.asBool,length_points,(length_points - 1.U))
            //Mux(Cat(0.U(1.W),write_banks_points) + 1.U >= bank_num.asUInt,write_length_points,write_length_points - 1.U)
        ))
        fifo_banks(i.asUInt).ar_addr := MuxLookup(i.asUInt,0.U,Seq(
            banks_points -> length_points,
            (banks_points + 1.U) -> Mux(banks_points.asBool,length_points + 1.U,length_points)
        ))
        fifo_banks(i.asUInt).in := MuxLookup(i.asUInt,0.U,Seq(
            banks_points -> io.write_in(0),
            banks_points + 1.U -> io.write_in(1)
        ))
        fifo_banks(i.asUInt).write := MuxLookup(i.asUInt,0.U,Seq(
            banks_points -> ((io.write_en === 2.U && io.read_en =/= 2.U) || (io.write_en === 1.U && io.read_en === 0.U)),
            (banks_points + 1.U) -> ((io.write_en === 2.U) && (io.read_en === 0.U))
        ))
    }
    when(io.write_en === io.read_en) {
        io.read_out := io.write_in
        banks_points := banks_points
        length_points := banks_points 
    }.elsewhen(io.write_en > io.read_en ){
        io.read_out := io.write_in
        banks_points := banks_points - (io.write_en - io.read_en)
        length_points := Mux((Cat(0.U(1.W),banks_points) - (io.write_en - io.read_en))(1),(length_points - 1.U),length_points)
    }.elsewhen(io.write_en === 0.U){
        io.read_out(0) := fifo_banks(0).out
        io.read_out(1) := fifo_banks(1).out
        banks_points := banks_points + (io.read_en - io.write_en)
        length_points := Mux((Cat(0.U(1.W),banks_points) + (io.read_en - io.write_en))(1),(length_points + 1.U),length_points) //如果加了之后符号位发生变化，说明发生了数据越界
    }.otherwise{//write_en = 1.U,read_en = 2.U
        io.read_out(0) := fifo_banks(0).out
        io.read_out(1) := io.write_in(0)
        banks_points := banks_points + 1.U
        length_points := Mux(banks_points.asBool,length_points + 1.U,length_points)
    }
    io.empty := length_points === (length - 1).asUInt && banks_points === 1.U 
    // io.empty := Mux(write_length_points === read_length_points,write_banks_points <= read_banks_points || write_banks_points <= read_banks_points - write_num.asUInt,
    //     Mux(write_length_points === read_length_points - 1.U,bank_num.asUInt - write_banks_points + read_banks_points <= write_num.asUInt ,0.U.asBool))

}


//这是free physical reg list，至少需要支持两读两写，建议直接用fifo来写这个东西

class reg_renaming extends Module {
      val io = IO(new Bundle { 
       
        val src_in    = Vec(4,Input(UInt(5.W)))
        val dest_in   = Vec(2,Input(UInt(5.W)))

        val dest_has = Input(UInt(2.W)) //都为1才代表两个指令都有目标指令
        val src_out   = Vec(4,Output(UInt(6.W)))
        val dest_out  = Vec(2,Output(UInt(6.W)))
        val retire_dest_in = Vec(2,Input(UInt(6.W)))
        val retire_dest = Input(UInt(2.W))

        val full      = Output(Bool())

     
    })
    
    val prf_table = Module(new two_ports_lookup_table(32,6)).io //physical register file table 代表啥寄存器对应啥物理寄存器
    val free_list = Module(new free_list(32,6)).io//就一个二路组相连的寄存器组而已
    val raw_matter  = (io.dest_in(0) ===  io.src_in(2) || io.dest_in(0) === io.src_in(3)) && io.dest_has(0) //read after write
    val waw_matter = (io.dest_in(0) === io.dest_in(1) ) && io.dest_has === "b11".U //write after write
    val true_dest_has = Mux(waw_matter,"b01".U,io.dest_has)
    
    //===================可以改=====================================================================
    prf_table.read_addr  := io.src_in //
    prf_table.write_addr := io.dest_in 
    prf_table.write_en   := true_dest_has
 
    free_list.read_en := MuxLookup(io.dest_has,1.U,Seq(
        3.U -> 2.U,
        0.U -> 0.U
    )) // 有多少需要有目标指令
    free_list.write_en := MuxLookup(io.retire_dest,1.U,Seq(
        3.U -> 2.U,
        0.U -> 0.U
    ))
    //======================================================    =====================================================
    io.full := free_list.empty //没有空余的物理寄存器了，肯定就renaming 满了，得停止流水线前面的部分
    io.src_out(0) := prf_table.read_out(0)
    io.src_out(1) := prf_table.read_out(1)
    io.src_out(2) := Mux(io.src_in(2) === io.dest_in(0) && io.dest_has(0),free_list.read_out(0),prf_table.read_out(2))
    io.src_out(3) := Mux(io.src_in(3) === io.dest_in(0) && io.dest_has(0),free_list.read_out(0),prf_table.read_out(3))

    when(io.dest_has === "b01".U) {
        io.dest_out(1) := free_list.read_out(0)
        prf_table.write_in(1) := free_list.read_out(0) 
        io.dest_out(0) := free_list.read_out(0)
        prf_table.write_in(0) := free_list.read_out(0)
    }.otherwise {
        io.dest_out := free_list.read_out
        prf_table.write_in := free_list.read_out
    }
    when(io.retire_dest === "b01".U) {
        free_list.write_in(0) := io.retire_dest_in(1)
        free_list.write_in(1) := io.retire_dest_in(0)
    }.otherwise{
        free_list.write_in   :=  io.retire_dest_in
    }
    

   // io.src_out(0) := io.src_in(0)

}
// object reg_renaming_test extends App{
//     (new ChiselStage).emitVerilog(new reg_renaming)
// }


