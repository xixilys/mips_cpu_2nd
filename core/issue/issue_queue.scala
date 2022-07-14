package mips_cpu_2nd

import chisel3._
import chisel3.stage._
import chisel3.util._
import firrtl.PrimOps
import scala.math._
import scala.reflect.runtime.Macros
import javax.swing.plaf.basic.BasicToolBarUI
import os.move
import os.read

class issue_port extends Bundle {
    val psc_src1 = Input(UInt(6.W))
    val psc_src2 = Input(UInt(6.W))
    val psc_dest = Input(UInt(6.W))
    val ready = Input(Bool())
    val opcode = Input(UInt(7.W))
    }

class regs_define extends Module {
    val io = IO(new Bundle{
        val code_write_en = Input(Bool())
        val ready_en = Input(Bool())
        val io_in= new issue_port
        val io_out = Flipped(new issue_port)

    })

    val this_reg = RegInit(0.U(25.W))
    val ready_reg = RegInit(0.U(1.W))
    this_reg := Mux(io.code_write_en,Cat(io.io_in.psc_src1,io.io_in.psc_src2,io.io_in.psc_dest,io.io_in.opcode),this_reg)
    ready_reg := Mux(io.code_write_en,io.io_in.ready,Mux(io.ready_en,1.U,ready_reg))
    // io.io_out.opcode := Mux(io.code_write_en,io_in.opcode,this_reg(6,0))
    // io.io_out.psc_dest := Mux(io.code_write_en,io_in.psc_dest,this_reg(12,7))
    // io.io_out.psc_src2 := Mux(io.code_write_en,io_in.psc_src2,this_reg(18,13))
    // io.io_out.psc_src1 :=Mux(io.code_write_en,io_in.psc_src1,this_reg(24,19))
    // io.io_out.ready := Mux(io.code_write_en,io_in.ready,ready_reg)
    //还是读优先好一点
    io.io_out.opcode  := this_reg(6,0)
    io.io_out.psc_dest := this_reg(12,7)
    io.io_out.psc_src2 := this_reg(18,13)
    io.io_out.psc_src1 := this_reg(24,19)
    io.io_out.ready := ready_reg
}
//================================================================================================================
//现在加了一个没啥用的功能，把所有的存储器配置成了写优先的状态，但是这样可能会增加延时，后面可能可以改一下
// 写优先貌似不是很行的样子
//已经改了，改成了读优先，但是不知道能不能用
//======================================================    ================    ==============================
//现在考虑的是采用压缩式还是采用非压缩式
//应该采用非数据捕捉式
//既然压缩式主要产生的问题是浪费布线面积啥的，只要不造成很大的延时就行

//规定，如果要单通道写的话，一定要在第一通道输入，不然数据会不正确
//并且对于发射来说，第一个通道对应着最小的发射地址值


class issue_queue (LENGTH:Int)extends Module { //发射队列
    val LENGTH_WIDTH = log2Up(LENGTH)
    val io = IO(new Bundle{//这个我是需要读所有的寄存器，是一个很麻烦的结构
        val inst_write = Input(UInt(2.W))
//        val inst_issue = Input(UInt(2.W))
        val inst_issue_num = Input(UInt(2.W))
        val inst_issue_addr = Vec(2,Input(UInt(LENGTH_WIDTH.W))) // 真正发射出去的指令地址
        val full = Output(Bool())
        val ready_en = Vec(LENGTH,Input(Bool()))
    })
    // def wire2bundle(num:UInt) : Data  = {
    //     val bundle_ly = IO(Flipped(new issue_port))
    //     bundle_ly.opcode  := num(6,0)
    //     bundle_ly.psc_dest := num(12,7)
    //     bundle_ly.psc_src2 := num(18,13)
    //     bundle_ly.psc_src1 := num(24,19)
    //     bundle_ly.ready := num(25)
    //     bundle_ly
    // }
    val issue_points = RegInit(0.U(((LENGTH_WIDTH+1).W)))
    val io_in= IO(Vec(2,( new issue_port))) //一次最多发射
    val io_out = IO(Vec(2,(Flipped(new issue_port))))//一次最多发射两个模块
    val issue_list =  VecInit(Seq.fill(LENGTH)(Module(new regs_define).io))
    // for(    i <- 0 until LENGTH) {  
    //     // issue_list(i).io.io_in:= Mux(issue_points === i.asUInt && io.inst_write(0) ,io_in(0),Mux((issue_points + 1.U) === i.asUInt && io.inst_write(1)
    //     //     ,io_in(1) ,Mux(move_list(i) === 2.U,issue_list(i+2).io_out,issue_list(i+1).io_out)))
        
    //     issue_list(i).io.code_write_en := move_list(i) =/= 0.U || (issue_points === i.asUInt && io.inst_write(0)) || (issue_points === i.asUInt && io.inst_write(1))
    // }
    val valid_points = issue_points  - io.inst_issue_num
    issue_points := valid_points + io.inst_write
    io.full :=  issue_points >= (LENGTH ).asUInt //至少留下三个空间不能放东西，不然不好做
    
    for( i <- 0 until LENGTH ) {
         issue_list(i).ready_en := io.ready_en(i)
    if(i < LENGTH - 2) {
        when( i.asUInt === valid_points ) {
            issue_list(i).io_in:= io_in(0)
            when(io.inst_write =/= 0.U) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
       }.elsewhen(i.asUInt === valid_points + 1.U){
            issue_list(i).io_in := io_in(1)
            when(io.inst_write === 2.U) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
       }.elsewhen((i.asUInt < io.inst_issue_addr(0) && io.inst_issue_num === 1.U) ||  (i.asUInt < io.inst_issue_addr(1) && i.asUInt < io.inst_issue_addr(0) )){
            issue_list(i).io_in := issue_list(i+1).io_out
            issue_list(i).code_write_en := 0.U  //不需要管这个情况下到底接的啥，不允许写入就行
       }.elsewhen(i.asUInt >= io.inst_issue_addr(0) && i.asUInt < io.inst_issue_addr(1) ) {//默认第一个发射的就是最小的
            when((io.inst_issue_addr(1) === (i + 1).U) ) {
                issue_list(i).io_in := issue_list(i+2).io_out
            }.otherwise{
                issue_list(i).io_in := issue_list(i+1).io_out
            }
            when(io.inst_issue_num =/= 0.U) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
       }.otherwise {
            when(io.inst_issue_num === 2.U) {
                issue_list(i).io_in := issue_list(i+2).io_out
                issue_list(i).code_write_en := 1.U
            }.elsewhen(io.inst_issue_num === 1.U){
                issue_list(i).io_in := issue_list(i+1).io_out
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).io_in := issue_list(i+1).io_out
                issue_list(i).code_write_en := 0.U
            }

       }
    }else if( i == LENGTH - 2) {
        when( i.asUInt === valid_points ) {
            issue_list(i).io_in := io_in(0)
            when(io.inst_write =/= 0.U) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
        }.elsewhen(i.asUInt === valid_points + 1.U){
            issue_list(i).io_in := io_in(1)
            when(io.inst_write === 2.U) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
        }.otherwise{
            issue_list(i).io_in := issue_list(i + 1).io_out
            when((io.inst_issue_num =/= 0.U && !(io.inst_issue_addr(0) === (i+1).U && io.inst_issue_num === 1.U) ) ) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
        }
    }else {
        when( i.asUInt === valid_points ) {
            issue_list(i).io_in := io_in(0)
            when(io.inst_write =/= 0.U) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
        }.elsewhen(i.asUInt === valid_points + 1.U){
            issue_list(i).io_in := io_in(1)
            when(io.inst_write === 2.U) {
                issue_list(i).code_write_en := 1.U
            }.otherwise{
                issue_list(i).code_write_en := 0.U
            }
        }.otherwise{
            issue_list(i).io_in := issue_list(i).io_out
            issue_list(i).code_write_en := 0.U
            
        }
    }
}
    io_out(0) := issue_list(io.inst_issue_addr(0)).io_out
    io_out(1) := issue_list(io.inst_issue_addr(1)).io_out

    // val answer_num1 = Wire(Vec(LENGTH,UInt(26.W)))
    // answer_num1(LENGTH  - 1) := Cat(issue_list(LENGTH - 1).io_out.ready,issue_list(LENGTH - 1).io_out.psc_dest, issue_list(LENGTH - 1).io_out.psc_src1, issue_list(LENGTH - 1).io_out.psc_src2,issue_list(LENGTH - 1).io_out.opcode)
    // for(i <- 0 until LENGTH - 1) {
    //     answer_num1(i.asUInt) := Mux( i.asUInt === io.inst_issue_addr(0),Cat(issue_list(i ).io_out.ready,issue_list(i ).io_out.psc_dest, 
    //     issue_list(i ).io_out.psc_src1, issue_list(i ).io_out.psc_src2,issue_list(i).io_out.opcode),answer_num1(i.asUInt + 1.U))
    // }
    // io_out(0) :=   (wire2bundle( answer_num1(0)))

    // val answer_num2 = Wire(Vec(LENGTH,UInt(26.W)))
    
    // answer_num2(LENGTH  - 1) := Cat(issue_list(LENGTH - 1).io_out.ready,issue_list(LENGTH - 1).io_out.psc_dest, issue_list(LENGTH - 1).io_out.psc_src1, issue_list(LENGTH - 1).io_out.psc_src2,issue_list(LENGTH - 1).io_out.opcode)
    // for(i <- 0 until LENGTH - 1) {
    //    answer_num2(i.asUInt) := Mux( i.asUInt === io.inst_issue_addr(1),Cat(issue_list(i ).io_out.ready,issue_list(i ).io_out.psc_dest, 
    //     issue_list(i ).io_out.psc_src1, issue_list(i ).io_out.psc_src2,issue_list(i ).io_out.opcode),answer_num2(i.asUInt + 1.U))   
    // } 
    // io_out(1) :=  wire2bundle( answer_num2(0))

}
object issue_queue_test extends App{
    (new ChiselStage).emitVerilog(new issue_queue(16))
}


