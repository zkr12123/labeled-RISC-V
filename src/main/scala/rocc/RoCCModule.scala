package custom_rocc

import chisel3._ 
import chisel3.util._ 
import freechips.rocketchip.tile._ //LazyRoCC
import freechips.rocketchip.config._ 
import freechips.rocketchip.diplomacy._ // LazyModule
import freechips.rocketchip.rocket._ // constants

/* RoCC busy will block pipeline */

class WithRoCCCacheTest extends Config( (site,here,up) => {
    case BuildRoCC => Seq(
        (p:Parameters) => {
            val roccCache = LazyModule(new RoccCacheTest(OpcodeSet.all)(p))
            roccCache
        }
    )
} )

class WithRoCCCacheTestPiped extends Config( (site,here,up) => {
    case BuildRoCC => Seq(
        (p:Parameters) => {
            val roccCache = LazyModule(new RoccCacheTestPiped(OpcodeSet.all)(p))
            roccCache
        }
    )
} )

class RoccCacheTest(opcode: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcode){

    val nRegBuffer = 1024
    override lazy val module = new LazyRoCCModuleImp(this) {
        
        // Buffer array of depth 1024
        val buffer = RegInit( VecInit(Seq.fill(nRegBuffer)(0.U(64.W))) )
        val i = RegInit(0.U(log2Ceil(nRegBuffer + 1).W))

        // RoCC decode logic and variables
        val len = RegInit(0.U(64.W))
        val nBytes = len << 3.U 
        val baseAddr = RegInit(0.U(64.W))
        val direction = RegInit(0.U(3.W))
        val busy = RegInit(false.B) // RoCC busy will block core pipeline

        val inst = io.cmd.bits.inst
        val dmaRead    = (inst.funct === 0.U) 
        val dmaWrite   = (inst.funct === 1.U) 
        val cacheRead  = (inst.funct === 2.U) 
        val cacheWrite = (inst.funct === 3.U) 
        io.cmd.ready := !busy 
        io.busy := busy
        when(io.cmd.fire()) {
            busy := true.B 
            i := 0.U 
            direction := MuxCase(0.U, Seq(
                (dmaRead    -> 0.U),
                (dmaWrite   -> 0.U),
                (cacheRead  -> 3.U),
                (cacheWrite -> 4.U))
            )
            len := io.cmd.bits.rs1
            baseAddr := io.cmd.bits.rs2
        }

        // cache interface control
        val memReq = io.mem.req
        val memResp = io.mem.resp
        val s_idle :: s_op :: Nil = Enum(2) // Cache_idle :: Cache_op
        // As Cache resp only has valid signal, need to track response to each request
        val state = RegInit(s_idle)
        val enCacheR = busy && (direction === 3.U) && (i < len)
        val enCacheW = busy && (direction === 4.U) && (i < len)

        memReq.valid := (enCacheR || enCacheW) && (state === s_idle)
        // memReq.valid := (enCacheR || enCacheW)
        memReq.bits.cmd := Mux(enCacheW, M_XWR, M_XRD)
        memReq.bits.addr := baseAddr 
        // memReq.bits.size := 
        memReq.bits.typ := "b011".U 
        memReq.bits.data := buffer(i)

        when(memReq.fire()) {state := s_op} // Cache receives request

        // Cache Read: Update baseAddr and buffer pointer after a successful read request
        when(enCacheR && memResp.valid && state === s_op) { //memResp valid, and current cache resp has not been processed
            baseAddr := baseAddr + 8.U 
            buffer(i) := memResp.bits.data 
            i := i + 1.U 
            state := s_idle
        }
        // Cache Write: Update base Addr after a successful write request
        when(enCacheW && state === s_op) {
            baseAddr := baseAddr + 8.U 
            i := i + 1.U 
            state := s_idle 
        }

        when(busy && !enCacheR && !enCacheW) { busy := false.B }
        // when(state===s_op && enCacheR && memResp.valid) {
        //     baseAddr := baseAddr + 8.U 
        //     i := i + 1.U 

        // }
        
    }

}

class RoccCacheTestPiped(opcode: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcode){

    val nRegBuffer = 1024
    override lazy val module = new LazyRoCCModuleImp(this) {
        
        // Buffer array of depth 1024
        val buffer = RegInit( VecInit(Seq.fill(nRegBuffer)(0.U(64.W))) )
        val i = RegInit(0.U(log2Ceil(nRegBuffer + 1).W)) // Track memReq
        val j = RegInit(0.U(log2Ceil(nRegBuffer + 1).W)) // Track memResp

        // RoCC decode logic and variables
        val len = RegInit(0.U(64.W))
        val baseAddr = RegInit(0.U(64.W))
        val direction = RegInit(0.U(3.W))
        val busy = RegInit(false.B)

        val inst = io.cmd.bits.inst
        val dmaRead    = (inst.funct === 0.U) 
        val dmaWrite   = (inst.funct === 1.U) 
        val cacheRead  = (inst.funct === 2.U) 
        val cacheWrite = (inst.funct === 3.U) 
        io.cmd.ready := !busy 
        io.busy := busy

        when(io.cmd.fire()) { // cmd.valid && cmd.ready
            busy := true.B 
            i := 0.U 
            j := 0.U
            direction := MuxCase(0.U, Seq(
                (dmaRead    -> 0.U),
                (dmaWrite   -> 0.U),
                (cacheRead  -> 3.U),
                (cacheWrite -> 4.U))
            )
            len := io.cmd.bits.rs1
            baseAddr := io.cmd.bits.rs2
        }

        // cache interface control
        val memReq = io.mem.req
        val memResp = io.mem.resp
        // val s_idle :: s_op :: Nil = Enum(2) // Cache_idle :: Cache_op
        // As Cache resp only has valid signal, need to track response to each request
        // val state = RegInit(s_idle)
        val enCacheR = busy && (direction === 3.U)
        val enCacheRReq = enCacheR && (i < len)
        val enCacheRResp = enCacheR && (j < len)
        val enCacheWReq = busy && (direction === 4.U) && (i < len)
        // val enCacheWResp = busy && (direction === 4.U) && (j < len)

        memReq.valid := (enCacheRReq || enCacheWReq)
        // memReq.valid := (enCacheR || enCacheW)
        memReq.bits.cmd := Mux(enCacheR, M_XRD, M_XWR)
        memReq.bits.addr := baseAddr 
        memReq.bits.tag := i // cache request tag, same tag will be returned by memResp to indicate correct sequence
        memReq.bits.typ := "b011".U 
        memReq.bits.data := buffer(i)

        // Cache Read
        //ReadRequest: update base addr and tag upon successful request
        when(memReq.fire() && enCacheRReq) {
            baseAddr := baseAddr + 8.U 
            i := i + 1.U
        }
        // Read Response: update buffer and tag tracker when cache resp tag matches
        // NOTE: j DOES NOT track out-of-order Cache response
        val respValid = memResp.valid && (memResp.bits.tag === j) 
        when(respValid && enCacheRResp) {
            buffer(j) := memResp.bits.data
            j := j + 1.U
        }

        // Cache Write
        // WriteRequest: update baseAddr and i upon successful write request
        when(memReq.fire() && enCacheWReq) {
            baseAddr := baseAddr + 8.U 
            i := i + 1.U
        }
        

        when(busy && !enCacheRReq && !enCacheRResp && !enCacheWReq) { busy := false.B }
        
    }

    
}