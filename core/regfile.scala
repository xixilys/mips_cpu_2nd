package mips_cpu_2nd

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
//四读二写状态
//===   ==========================  =========================    =====
//这是一个双写四读的逻辑寄存器堆，用来做逻辑寄存器用的，同时也为了方便进行diff test
//这是一个双发射乱序处理器的寄存器堆
//=== ========================== =========================  ========================= =================
class regfile extends Module {
        
    val io = IO(new Bundle { //有隐式的时钟与复位，并且复位为高电平复位

        val A1   = Vec(2,Input(UInt(5.W)))//rs
        val A2   = Vec(2,Input(UInt(5.W)))//rt
        val WE3   = Input(UInt(2.W))//write enables 
        val A3   = Vec(2,Input(UInt(5.W)))//rd
        val WD3   = Vec(2,Input(UInt(32.W)))//write data

        val RD1    = Vec(2,Output(UInt(32.W))) //rs data
        val RD2   = Vec(2,Output(UInt(32.W))) //rt data
        
    })
    val regs_table = Module(new two_ports_lookup_table(32,32)).io
    //A3 不能等于 0
    regs_table.read_addr(0) := io.A1(0)
    io.RD1(0) := regs_table.read_out(0)
    regs_table.read_addr(1) := io.A2(0)
    io.RD2(0) := regs_table.read_out(1)
    regs_table.read_addr(2) := io.A1(1)
    io.RD1(1) := regs_table.read_out(2)
    regs_table.read_addr(3) := io.A2(1)
    io.RD2(1) := regs_table.read_out(3)

    regs_table.write_in := io.WD3
    regs_table.write_en := io.WE3
    regs_table.write_addr := io.A3
}

object regfile_test extends App{
    (new ChiselStage).emitVerilog(new regfile )
}
