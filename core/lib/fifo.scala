package example.mips_cpu_2nd

import chisel3._
import chisel3.stage._
import chisel3.util._
import firrtl.PrimOps
import scala.math._
import scala.reflect.runtime.Macros
import javax.swing.plaf.basic.BasicToolBarUI



class  fifo   extends Module  with mips_macros{
   
    val io = IO(new Bundle { //分支指令不支持同时写
        pc_in
        val pc = Input(UInt(32.W)) //一般是支持同时判断两条指令pc和pc+4和pc+8，而对于处于八位边界的指令，还是和cache一样不做处理，只发射一条指令，我想要cache支持一次出三条了，草
        val pc_plus = Input(UInt(32.W)) 
        val pc_plus_plus = Input(UInt(32.W))
        val write_pc = Input(UInt(32.W))
        val aw_pht_ways_addr = Input(UInt(4.W))
        val aw_pht_addr = Input(UInt(7.W))
        val aw_bht_addr   = Input(UInt(7.W))
        val aw_target_addr = Input(UInt(32.W))
        val btb_write = Input(Bool())
        val bht_write = Input(Bool())
        val pht_write = Input(Bool()) 
        val bht_in = Input(UInt(7.W))
        val pht_in = Input(UInt(2.W))
        val out_L = Output(UInt(2.W))
        val out_M = Output(UInt(2.W))
        val out_R = Output(UInt(2.W))
        val pre_L = Output(UInt(1.W))
        val pre_M = Output(UInt(1.W))
        val pre_R = Output(UInt(1.W))
        val bht_L = Output(UInt(7.W)) 
        val bht_M = Output(UInt(7.W))
        val bht_R = Output(UInt(7.W))
        val pre_target_L = Output(UInt(32.W))
        val pre_target_M = Output(UInt(32.W))
        val pre_target_R = Output(UInt(32.W))
    })//相当于要查两个表，不知道延迟会到多高
}