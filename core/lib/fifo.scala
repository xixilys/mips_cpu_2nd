package mips_cpu_2nd

import chisel3._
import chisel3.stage._
import chisel3.util._
import firrtl.PrimOps
import scala.math._
import scala.reflect.runtime.Macros
import javax.swing.plaf.basic.BasicToolBarUI


//想参数化，但是貌似不是很好参数化的样子，但是还好，直接粘贴过去改一下就行
class  fifo  (length :Int,width :Int,write_num:Int,read_num:Int) extends Module  with mips_macros{
    val bank_width = log2Up(write_num.max(read_num))
    val bank_num  = pow(2,bank_width).toInt
    val length_width = (log10(length)/log10(2)).toInt
    val io = IO(new Bundle { 
        val read_en = Input(UInt(bank_width.W))
        val write_en = Input(UInt(bank_width.W))//0为前面的
        val read_out  = Vec(read_num,Output(UInt(width.W)))//0为前面的
        val write_in  = Vec(write_num,Input(UInt(width.W)))//0为前面的
        val full = Output(Bool()) //浪费一点空间无所谓，只要剩余的空间小于最大的写入空间，就算满
        // val empty = Output(Bool()) //只有满足超过发射大小的情况下才叫做不空 ,不需要empty判定吧，我这个算是写优先】的效果
        //感觉应该还算比较难满的把 感觉
    })
    val fifo_banks = VecInit(Seq.fill(bank_num)(Module(new Look_up_table(length,width)).io))
    val write_banks_points = RegInit(0.U(bank_width.W))
    val write_length_points = RegInit(0.U(length_width.W))
    val read_banks_points = RegInit(0.U(bank_width.W))
    val read_length_points = RegInit(0.U(length_width.W))
    
    for(i <- 0 until bank_num) {
        fifo_banks(i.asUInt).aw_addr := MuxLookup(i.asUInt,0.U,Seq(
            write_banks_points -> write_length_points,
            write_banks_points + 1.U -> Mux(Cat(0.U(1.W),write_banks_points) + 1.U > bank_num.asUInt,write_length_points,write_length_points + 1.U),
            write_banks_points + 2.U -> Mux(Cat(0.U(1.W),write_banks_points) + 2.U > bank_num.asUInt,write_length_points,write_length_points + 1.U)
        ))
        fifo_banks(i.asUInt).ar_addr := MuxLookup(i.asUInt,0.U,Seq(
            read_banks_points -> write_length_points,
            read_banks_points + 1.U -> Mux(Cat(0.U(1.W),read_banks_points) + 1.U > bank_num.asUInt,read_length_points,read_length_points + 1.U),
            read_banks_points + 2.U -> Mux(Cat(0.U(1.W),read_banks_points) + 2.U > bank_num.asUInt,read_length_points,read_length_points + 1.U)
        ))
        fifo_banks(i.asUInt).in := MuxLookup(i.asUInt,0.U,Seq(
            read_banks_points -> io.write_in(0),
            read_banks_points + 1.U -> io.write_in(1),
            read_banks_points + 2.U -> io.write_in(2)
        ))
        fifo_banks(i.asUInt).write := MuxLookup(i.asUInt,0.U,Seq(
            write_banks_points -> 1.U.asBool,
            write_banks_points + 1.U -> 1.U.asBool,
            write_banks_points + 2.U -> 1.U.asBool
        ))


        // fifo_banks(write_banks_points + i.asUInt).aw_addr :=    (write_length_points   + Mux((Cat(0.U(1.W),write_banks_points) + i.asUInt)(bank_width),1.U,0.U))(length_width - 1,0)
        // fifo_banks(write_banks_points + i.asUInt).ar_addr :=    (read_length_points +  Mux((Cat(0.U(1.W),read_banks_points) + i.asUInt)(bank_width),1.U,0.U))(length_width - 1,0)
        // fifo_banks(write_banks_points + i.asUInt).write   :=    i.asUInt < io.write_en
        // fifo_banks(write_banks_points + i.asUInt).in      :=    io.write_in(i)
    }
    write_banks_points := (write_banks_points + io.write_en)(bank_width - 1,0)
    write_length_points :=  (write_length_points + Mux((Cat(0.U(1.W),write_banks_points) + io.write_en)(bank_width),1.U,0.U))(length_width - 1,0)
    read_banks_points := (read_banks_points + io.read_en)(bank_width - 1,0)
    read_length_points :=  (read_length_points + Mux((Cat(0.U(1.W),read_banks_points) + io.read_en)(bank_width),1.U,0.U))(length_width - 1,0)
    
    io.full := Mux(write_length_points === read_length_points,write_banks_points >= read_banks_points || write_banks_points <= read_banks_points - write_num.asUInt,
        Mux(write_length_points === read_length_points - 1.U,bank_num.asUInt - write_banks_points + read_banks_points <= write_num.asUInt ,0.U.asBool))
    
    for(i <- 0 until read_num) {
        io.read_out(i.asUInt) := fifo_banks(read_banks_points + i.asUInt).out
    }
    // io.empty := Mux(write_length_points === read_length_points,write_banks_points <= read_banks_points || write_banks_points <= read_banks_points - write_num.asUInt,
    //     Mux(write_length_points === read_length_points - 1.U,bank_num.asUInt - write_banks_points + read_banks_points <= write_num.asUInt ,0.U.asBool))

    




}

object fifo_test extends App{
    (new ChiselStage).emitVerilog(new fifo(128,32,3,2))
}


