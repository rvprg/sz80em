namespace FSpectrum
open System
open System.IO
open System.Drawing
open System.Drawing.Imaging
open Microsoft.FSharp.NativeInterop
open System.Windows.Forms
open System.Threading

type IPort =
    abstract read: uint16 -> byte

type KeyCode = { row: int; bit: uint8; }

type public IKeyboard =
    abstract KeyPressed: Keys -> unit
    abstract KeyReleased: Keys -> unit

type KeyboardMsg =
    | KeyPressed of Keys
    | KeyReleased of Keys
    | ReadKeys of uint16 * AsyncReplyChannel<byte>

type Keyboard() = class
    let ports = Array.create 8 0xFFuy
    let keyMap = 
      Map.empty. 
          Add(Keys.ShiftKey, [ { row = 0; bit = 0x01uy } ]).
          Add(Keys.Z, [ { row = 0; bit = 0x02uy } ]).
          Add(Keys.X, [ { row = 0; bit = 0x04uy } ]).
          Add(Keys.C, [ { row = 0; bit = 0x08uy } ]).
          Add(Keys.V, [ { row = 0; bit = 0x10uy } ]).
          Add(Keys.A, [ { row = 1; bit = 0x01uy } ]).
          Add(Keys.S, [ { row = 1; bit = 0x02uy } ]).
          Add(Keys.D, [ { row = 1; bit = 0x04uy } ]).
          Add(Keys.F, [ { row = 1; bit = 0x08uy } ]).
          Add(Keys.G, [ { row = 1; bit = 0x10uy } ]).
          Add(Keys.Q, [ { row = 2; bit = 0x01uy } ]).
          Add(Keys.W, [ { row = 2; bit = 0x02uy } ]).
          Add(Keys.E, [ { row = 2; bit = 0x04uy } ]).
          Add(Keys.R, [ { row = 2; bit = 0x08uy } ]).
          Add(Keys.T, [ { row = 2; bit = 0x10uy } ]).
          Add(Keys.D1, [ { row = 3; bit = 0x01uy } ]).
          Add(Keys.D2, [ { row = 3; bit = 0x02uy } ]).
          Add(Keys.D3, [ { row = 3; bit = 0x04uy } ]).
          Add(Keys.D4, [ { row = 3; bit = 0x08uy } ]).
          Add(Keys.D5, [ { row = 3; bit = 0x10uy } ]).
          Add(Keys.D0, [ { row = 4; bit = 0x01uy } ]).
          Add(Keys.D9, [ { row = 4; bit = 0x02uy } ]).
          Add(Keys.D8, [ { row = 4; bit = 0x04uy } ]).
          Add(Keys.D7, [ { row = 4; bit = 0x08uy } ]).
          Add(Keys.D6, [ { row = 4; bit = 0x10uy } ]).
          Add(Keys.P, [ { row = 5; bit = 0x01uy } ]).
          Add(Keys.O, [ { row = 5; bit = 0x02uy } ]).
          Add(Keys.I, [ { row = 5; bit = 0x04uy } ]).
          Add(Keys.U, [ { row = 5; bit = 0x08uy } ]).
          Add(Keys.Y, [ { row = 5; bit = 0x10uy } ]).
          Add(Keys.Enter, [ { row = 6; bit = 0x01uy} ]).
          Add(Keys.L, [ { row = 6; bit = 0x02uy } ]).
          Add(Keys.K, [ { row = 6; bit = 0x04uy } ]).
          Add(Keys.J, [ { row = 6; bit = 0x08uy } ]).
          Add(Keys.H, [ { row = 6; bit = 0x10uy } ]).
          Add(Keys.Space, [ { row = 7; bit = 0x01uy } ]).
          Add(Keys.ControlKey, [ { row = 7; bit = 0x02uy } ]).
          Add(Keys.M, [ { row = 7; bit = 0x04uy } ]).
          Add(Keys.N, [ { row = 7; bit = 0x08uy } ]).
          Add(Keys.B, [ { row = 7; bit = 0x10uy } ]).
          Add(Keys.Escape, [ { row = 0; bit = 0x01uy }; 
                             { row = 7; bit = 0x01uy } ]).
          Add(Keys.Back, [ { row = 0; bit = 0x01uy }; 
                           { row = 4; bit = 0x01uy } ]).
          Add(Keys.Capital, [ { row = 0; bit = 0x01uy }; 
                              { row = 3; bit = 0x02uy } ]).
          Add(Keys.Oemplus, [ { row = 7; bit = 0x02uy }; 
                              { row = 6; bit = 0x02uy } ]) // ctrl + L


    let processor = 
      MailboxProcessor.Start(fun inbox ->
          let rec nextMsg =
              async {
                  let! msg = inbox.Receive()
                  match msg with
                  | KeyPressed key -> 
                      if keyMap.ContainsKey(key) then
                          for k in keyMap.Item(key) do
                             ports.[k.row] <- ports.[k.row] &&& (k.bit ^^^ 0xFFuy)
                      return! nextMsg
                  | KeyReleased key -> 
                      if keyMap.ContainsKey(key) then
                          for k in keyMap.Item(key) do
                             ports.[k.row] <- ports.[k.row] ||| k.bit
                      return! nextMsg
                  | ReadKeys (port, reply) ->
                      let result = 
                          (byte (port >>> 8)) 
                          |> Seq.unfold (fun i -> Some(i, i >>> 1)) 
                          |> Seq.take 8 
                          |> Seq.mapi (fun i port -> if port &&& 0x01uy = 0uy then ports.[i] else 0uy) 
                          |> Seq.filter(fun i -> i > 0uy) 
                          |> Seq.fold (fun r v -> r &&& v) 0xFFuy 
                      reply.Reply(result)
                      return! nextMsg
              }
          nextMsg
          )


    interface IKeyboard with
        member x.KeyPressed e = processor.Post(KeyPressed(e))
        member x.KeyReleased e = processor.Post(KeyReleased(e))

    interface IPort with
        member x.read port = processor.PostAndReply(
                                 (fun reply -> ReadKeys(port, reply)), 
                                    timeout = 200)
end

type DirectBitmapAccess(bitmap: Bitmap) =
     let data = bitmap.LockBits(
                      new Rectangle(0, 0, bitmap.Width, bitmap.Height), 
                      ImageLockMode.WriteOnly, 
                      bitmap.PixelFormat)

     let setPixel x y (r,g,b) = 
        let address = 
            NativePtr.add<byte> (NativePtr.ofNativeInt data.Scan0) 
                        ((y * data.Stride) + (x * 3))
        NativePtr.write address b
        NativePtr.set address 1 g
        NativePtr.set address 2 r

     member this.SetPixel(x, y, color: Color) = 
        setPixel x y (color.R, color.G, color.B)

     interface IDisposable with
        member this.Dispose() =
            bitmap.UnlockBits(data)

type IVideoMemoryProcessor =
    abstract ChangeFlash: unit
    abstract DrawImage: byte[] -> unit
    abstract GetImage: Image

type VideoMemoryProcessorMsg =
    | DrawImage of byte[]
    | GetImage of AsyncReplyChannel<Bitmap>
    | Flash

type VideoMemoryProcessor() as this = class
    let WIDTH = 256;
    let HEIGHT = 192;
    let CHAR_WIDTH_HEIGHT = 8;
    let WIDTH_IN_CHARS = WIDTH / CHAR_WIDTH_HEIGHT;
    let HEIGHT_IN_CHARS = HEIGHT / CHAR_WIDTH_HEIGHT;
    let SCREEN_MEMORY_SIZE = WIDTH * HEIGHT / CHAR_WIDTH_HEIGHT;

    let CONTENDED_MEMORY_START = 0x4000
    let screens = 
      Array.init 2 (fun i -> new Bitmap(WIDTH, HEIGHT, PixelFormat.Format24bppRgb))
    let mutable screen_index = 0
    let mutable flash = 0x7Fuy;


    let processor = 
      MailboxProcessor.Start(fun inbox ->
          let rec nextMsg =
              async {
                  let! msg = inbox.Receive()
                  match msg with
                  | GetImage(reply) -> 
                      reply.Reply(screens.[(screen_index + 1) % 2])
                      return! nextMsg
                  | DrawImage bitmap -> 
                      this.drawFrame bitmap
                      screen_index <- (screen_index + 1) % 2
                      return! nextMsg
                  | Flash ->
                      flash <- if flash = 0x7Fuy then 0xFFuy else 0x7Fuy
                      return! nextMsg
              }
          nextMsg
          )


    let COLORS =
      Map.empty. 
          Add(0b0000, Color.FromArgb(0xFF, 0x00, 0x00, 0x00)).
          Add(0b1110, Color.FromArgb(0xFF, 0xFF, 0xFF, 0x00)).
          Add(0b0001, Color.FromArgb(0xFF, 0x00, 0x00, 0xC0)).
          Add(0b1001, Color.FromArgb(0xFF, 0x00, 0x00, 0xFF)).
          Add(0b0010, Color.FromArgb(0xFF, 0xC0, 0x00, 0x00)).
          Add(0b1010, Color.FromArgb(0xFF, 0xFF, 0x00, 0x00)).
          Add(0b0011, Color.FromArgb(0xFF, 0xC0, 0x00, 0xC0)).
          Add(0b1011, Color.FromArgb(0xFF, 0xFF, 0x00, 0xFF)).
          Add(0b0100, Color.FromArgb(0xFF, 0x00, 0xC0, 0x00)).
          Add(0b1000, Color.FromArgb(0xFF, 0x00, 0x00, 0x00)).
          Add(0b0101, Color.FromArgb(0xFF, 0x00, 0xC0, 0xC0)).
          Add(0b1101, Color.FromArgb(0xFF, 0x00, 0xFF, 0xFF)).
          Add(0b0110, Color.FromArgb(0xFF, 0xC0, 0xC0, 0x00)).
          Add(0b1100, Color.FromArgb(0xFF, 0x00, 0xFF, 0x00)).
          Add(0b0111, Color.FromArgb(0xFF, 0xC0, 0xC0, 0xC0)).
          Add(0b1111, Color.FromArgb(0xFF, 0xFF, 0xFF, 0xFF)) 


    member private x.drawFrame(memory: byte[]) =
      using (new DirectBitmapAccess(screens.[screen_index])) (fun lockContext ->
          for i in CONTENDED_MEMORY_START..(CONTENDED_MEMORY_START + SCREEN_MEMORY_SIZE - 1) do
             let address = i - CONTENDED_MEMORY_START // 0x4000
             let row = ((address &&& 0b0001100000000000) >>> 8) ||| ((address &&& 0b11100000) >>> 5)
             let col = (address &&& 0b11111)
             let line = ((address &&& 0b11100000000) >>> 8)

             let attribute = memory.[CONTENDED_MEMORY_START
                      + SCREEN_MEMORY_SIZE + row * WIDTH_IN_CHARS + col] &&& flash
             let mutable ink = int ((attribute &&& 0b111uy) ||| ((attribute &&& 0b1000000uy) >>> 3))
             let mutable paper = int ((attribute &&& 0b1111000uy) >>> 3)

             if (attribute &&& 0x80uy) <> 0uy then
                let tmp = ink
                ink <- paper
                paper <- tmp

             let mutable x = 0
             let mutable mask = 0b10000000uy
             while mask > 0uy do
                let xx = (col * CHAR_WIDTH_HEIGHT + x);
                let yy = row * CHAR_WIDTH_HEIGHT + line;
                let clr = COLORS.[if (memory.[i] &&& mask) = 0uy then paper else ink]
                lockContext.SetPixel(xx, yy, clr)
                mask <- mask >>> 1
                x <- x + 1


      )


    interface IVideoMemoryProcessor with
        member x.ChangeFlash = processor.Post(Flash)
        member x.DrawImage bitmap = processor.Post(DrawImage bitmap)
        member x.GetImage = 
                   processor.PostAndReply(
                        (fun reply -> GetImage(reply)), timeout = 200) :> Image
end

type IDisplay =
    abstract draw: byte[] -> uint16 -> unit

type Display(videoProcessor: IVideoMemoryProcessor) as this = class
    inherit Panel(Dock = DockStyle.Fill)

    do
        this.DoubleBuffered <- true
        this.Visible <- true

    override x.OnPaint e =
        e.Graphics.DrawImage(videoProcessor.GetImage, 0, 0)

    interface IDisplay with
        member x.draw (memory: byte[]) frame =
            if frame % 16us = 0us then
                videoProcessor.ChangeFlash
            videoProcessor.DrawImage memory
            x.Invalidate()
end

exception InstructionNotSupported of string
type Z80(keyboard: IPort) as this = class
    let A = 0
    let F = 1
    let B = 2
    let C = 3
    let D = 4
    let E = 5
    let H = 6
    let L = 7
    let AS = 8
    let FS = 9
    let BS = 10
    let CS = 11
    let DS = 12
    let ES = 13
    let HS = 14
    let LS = 15

    let regs = [B; C; D; E; H; L; A]

    let IX = 0
    let IY = 1

    let S_FLAG = 0b10000000uy
    let Z_FLAG = 0b01000000uy
    let X5_FLAG = 0b00100000uy
    let H_FLAG = 0b00010000uy
    let X3_FLAG = 0b00001000uy
    let P_FLAG = 0b00000100uy
    let V_FLAG = 0b00000100uy
    let N_FLAG = 0b00000010uy
    let C_FLAG = 0b00000001uy
    let X53_FLAGS = X5_FLAG ||| X3_FLAG
    let SZ_FLAGS = S_FLAG ||| Z_FLAG
    let SZP_FLAGS = SZ_FLAGS ||| P_FLAG
    let SZHN_FLAGS = SZ_FLAGS ||| H_FLAG ||| N_FLAG

    let mutable ir = 0us
    let mutable sp = 0us
    let mutable pc = 0us

    let register = Array.create 16 0uy

    let index_register = Array.create 2 0us

    let memory = Array.create (0xFFFF+1) 0xFFuy

    let precomputed_flags = 
          Array.create 256 0uy |> Array.mapi (
                    fun i v -> (if i > 0x7F then S_FLAG else 0uy) ||| 
                               ((byte i) &&& X53_FLAGS) ||| 
                               (if i = 0 then Z_FLAG else 0uy))

    let sz53n_add = precomputed_flags
    let sz53n_sub = precomputed_flags |> Array.map (fun v -> v ||| N_FLAG)

    let powers_of_two = 1 |> Seq.unfold (fun i -> Some(i, i <<< 1))
    let parity v len = powers_of_two |> 
                        Seq.take len |> 
                        Seq.fold (fun e i -> if i &&& v <> 0 then not e else e) true
    let parityByte v = parity v 8

    let sz53pn_add = sz53n_add |> Array.mapi (fun i v -> v ||| (if parityByte i then P_FLAG else 0uy))
    let sz53pn_sub = sz53n_sub |> Array.mapi (fun i v -> v ||| (if parityByte i then P_FLAG else 0uy))

    let halfcarry_add_table = [| false; true; true; true; false; false; false; true |]
    let halfcarry_sub_table = [| false; false; true; false; true; false; true; true |]

    let overflow_add_table = [| false; false; false; true; true; false; false; false |]
    let overflow_sub_table = [| false; true; false; false; false; false; true; false |]

    let SCREEN_CONTENTS = 128
    let LEFT_BORDER = 24
    let RIGHT_BORDER = 24
    let RETRACE = 48
    let SCREEN_CONTENTS_AND_BORDER_AND_RETRACE = 
            SCREEN_CONTENTS + RIGHT_BORDER + LEFT_BORDER + RETRACE
    let TOTAL_LINES = 192
    let PATTERN_LENGTH = 8
    let TSTATE_START = 14335
    let TSTATE_END = 
            TSTATE_START + SCREEN_CONTENTS_AND_BORDER_AND_RETRACE * TOTAL_LINES - 1
    let CONTENDED_MEMORY_START = 0x4000us
    let CONTENDED_MEMORY_END = 0x7FFFus
    let FRAME_CYCLES_TOTAL = 69888

    let mutable tstates = 0
    let mutable frames = 0us

    let mutable im = 0
    let mutable ei = false
    let mutable iff1 = false
    let mutable iff2 = false
    let mutable halt = false
    let mutable intr = false

    let MEMORY_READ_TIME = 3
    let MEMORY_WRITE_TIME = 3

    let OP_FETCH_TIME = 4

    let MASK_8 = 0xFFuy

    let operations = 
        Array.init 256 (fun i -> 
            (fun () -> raise (InstructionNotSupported("Unknown Instruction")) ))
    let operations_extended = 
        Array.init 256 (fun i -> 
            (fun () -> raise (InstructionNotSupported("Unknown Extended Instruction")) ))
    let operations_bits = 
        Array.init 256 (fun i -> 
            (fun () -> raise (InstructionNotSupported("Unknown Bit Instruction")) ))
    let operations_indexes = 
        Array.init 256 (fun i -> 
            (fun (ireg: int) -> raise (InstructionNotSupported("Unknown Index Instruction")) ))
    let operations_indexes_bits = 
        Array.init 256 (fun i -> (fun (address: uint16) (value: byte) -> 
                raise (InstructionNotSupported("Unknown Index Bit Instruction")) ))

    do
        this.initializeOperations
        this.initializeExtendedOperations
        this.initializeBitOperations
        this.initializeIndexOperations
        this.initializeIndexBitOperations

    member x.Memory
       with get() = memory

    member x.Tstates
       with get() = tstates

    member x.Frames
       with get() = frames

    member x.R
         with get() = byte ir
         and set(v: byte) = ir <- (ir &&& 0xFF00us) ||| (uint16 v)

    member x.I
        with get() = byte (ir >>> 8)
        and set(v: byte) = ir <- (ir &&& 0x00FFus) ||| ((uint16 v) <<< 8)

    member x.Iff1
        with get() = iff1
        and set(v) = iff1 <- v

    member x.Iff2
        with get() = iff2
        and set(v) = iff2 <- v

    member x.loadRom(romFile: String) =
        let rom = File.ReadAllBytes(romFile)
        rom.CopyTo(memory, 0)

    member x.get_contention_delay tstate = 
          match tstate with
            | tstate when tstate < TSTATE_START || tstate > TSTATE_END -> 0
            | _ ->  let b_m = (tstate - TSTATE_START) % SCREEN_CONTENTS_AND_BORDER_AND_RETRACE
                    if b_m >= SCREEN_CONTENTS then 0 else 
                        let delay = (PATTERN_LENGTH - (b_m % PATTERN_LENGTH)) - 2
                        if delay > 0 then delay else 0

    member x.check_memory_contention (addr: uint16) = 
        tstates <- tstates + (if addr >= CONTENDED_MEMORY_START && addr <= CONTENDED_MEMORY_END 
                              then x.get_contention_delay tstates 
                              else 0)

    member x.check_memory_contention_m (addr: uint16) times =
        if addr >= CONTENDED_MEMORY_START && addr <= CONTENDED_MEMORY_END then
            for i = 1 to times do
                  tstates <- tstates + 1 + x.get_contention_delay tstates
        else
            tstates <- tstates + times

    member x.write_to_port (port: uint16) (value: byte) =
        x.check_memory_contention port
        tstates <- tstates + 1

        if port &&& 0x0001us <> 0us then
            x.check_memory_contention_m port 3
        else
            x.check_memory_contention port
            tstates <- tstates + 3

    member x.read_from_port (port: uint16) =
       x.check_memory_contention port
       tstates <- tstates + 1

       if port &&& 0x0001us <> 0us then
           x.check_memory_contention_m port 3
       else
           x.check_memory_contention port
           tstates <- tstates + 3

       if port &&& 0x00FFus = 0xFEus then
           keyboard.read port
       else
           0xFFuy

    member x.read_from_memory (addr: uint16) =
        x.check_memory_contention addr
        tstates <- tstates + MEMORY_READ_TIME
        memory.[(int)addr]

    member x.write_to_memory (addr: uint16) (data: byte) =
        x.check_memory_contention addr
        tstates <- tstates + MEMORY_WRITE_TIME
        memory.[(int)addr] <- data

    member x.increment_memory_refresh_register = 
        let r = ir &&& 0x00FFus
        let r = (r &&& 0b10000000us) ||| ((r + 1us) &&& 0b1111111us)
        ir <- (ir &&& 0xFF00us) ||| r

    member x.get_compound (r1: byte) (r2: byte) = (uint16 r1 <<< 8) ||| (uint16 r2)

    member x.push_to_stack (v: uint16) =
        sp <- sp - 1us
        x.write_to_memory sp (byte (v >>> 8))
        sp <- sp - 1us
        x.write_to_memory sp (byte v)

    member x.pop_from_stack =
        let addr1 = x.read_from_memory sp
        sp <- sp + 1us
        let addr2 = x.read_from_memory sp
        sp <- sp + 1us
        x.get_compound addr2 addr1

    member x.interrupt =
        if halt then
            halt <- false
            pc <- pc + 1us

        tstates <- tstates + 7
        x.increment_memory_refresh_register
        iff1 <- false 
        iff2 <- false

        x.push_to_stack pc

        if im = 2 then
            let i = (ir &&& 0xFF00us) >>> 8 
            let p = i <<< 8 ||| 0xFFus
            let v1 = x.read_from_memory p
            let v2 = x.read_from_memory (p + 1us)
            let v = x.get_compound v2 v1
            pc <- v
        else
            pc <- 0x38us

    member x.read_next_instruction =
        x.check_memory_contention pc
        tstates <- tstates + OP_FETCH_TIME
        memory.[(int)pc]

    member public x.execute_next_instruction() =
        x.increment_memory_refresh_register
        let op = x.read_next_instruction
        pc <- pc + 1us
        operations.[(int)op]()

    member x.execute utstates =
        while tstates < utstates do
            if intr && iff1 && (not ei) then
                x.interrupt
            x.execute_next_instruction()
            if ei then ei <- false 

    member x.execute_frame =
        if tstates < 32 then
            intr <- true
            x.execute 32
        intr <- false
        x.execute(FRAME_CYCLES_TOTAL)
        tstates <- tstates % FRAME_CYCLES_TOTAL
        frames <- frames + 1us
        memory

    member x.initializeOperations = 
        operations.[0x00] <- fun () -> ()

        operations.[0x07] <- fun () ->
            let cf = if (register.[A] &&& 0x80uy) <> 0uy then C_FLAG else 0uy
            register.[A] <- (register.[A] <<< 1) ||| cf
            register.[F] <- (register.[F] &&& SZP_FLAGS) ||| (register.[A] &&& X53_FLAGS) ||| cf

        operations.[0x08] <- fun () ->
            x.exchange_registers A AS
            x.exchange_registers F FS

        operations.[0x0F] <- fun () ->
            let cf = register.[A] &&& 0x01uy
            register.[A] <- (register.[A] >>> 1) ||| (cf <<< 7)
            register.[F] <- (register.[F] &&& SZP_FLAGS) ||| (register.[A] &&& X53_FLAGS) ||| cf

        operations.[0x10] <- fun () ->
            x.check_memory_contention_m ir 1
            register.[B] <- (register.[B] - 1uy)
            if register.[B] <> 0uy then
                let offset = x.get_signed_byte(x.read_from_memory pc)
                x.check_memory_contention_m pc 5
                pc <- uint16 (int pc + offset + 1)
            else
                pc <- pc + 1us

        operations.[0x16] <- fun () ->
             register.[D] <- x.read_from_memory pc
             pc <- pc + 1us

        operations.[0x17] <- fun () ->
            let cf = if (register.[A] &&& 0x80uy) <> 0uy then C_FLAG else 0uy
            register.[A] <- ((register.[A] <<< 1) &&& 0xFEuy) ||| (register.[F] &&& C_FLAG)
            register.[F] <- (register.[F] &&& SZP_FLAGS) ||| (register.[A] &&& X53_FLAGS) ||| cf

        operations.[0x18] <- fun () ->
            let offset = x.get_signed_byte(x.read_from_memory pc)
            x.check_memory_contention_m pc 5
            pc <- uint16 (int pc + offset + 1)

        operations.[0x1F] <- fun () ->
            let cf = if register.[A] &&& 0x01uy <> 0uy then C_FLAG else 0uy
            register.[A] <- ((register.[A] >>> 1) &&& MASK_8) ||| ((register.[F] &&& C_FLAG) <<< 7)
            register.[F] <- (register.[F] &&& SZP_FLAGS) ||| (register.[A] &&& X53_FLAGS) ||| cf

        operations.[0x22] <- fun () ->
            let offset = x.read_address_pointing_by_pc
            x.write_to_memory offset register.[L]
            x.write_to_memory (offset + 1us) register.[H]

        operations.[0x27] <- fun () ->
            let cf = register.[F] &&& C_FLAG
            let correction = if (register.[F] &&& C_FLAG <> 0uy) || (register.[A] > 0x99uy) 
                                then 0x60uy else 0x00uy
            let correction = correction ||| 
                                (if ((register.[F] &&& H_FLAG <> 0uy) || 
                                        ((register.[A] &&& 0x0Fuy) > 0x09uy)) 
                                            then 0x06uy else 0x00uy)
            let cf = if register.[A] > 0x99uy then C_FLAG else cf
            if register.[F] &&& N_FLAG <> 0uy then
                x.sub_from_register A correction
                register.[F] <- sz53pn_sub.[int register.[A]] ||| (register.[F] &&& H_FLAG) ||| cf
            else
                x.add_to_register A correction
                register.[F] <- sz53pn_add.[int register.[A]] ||| (register.[F] &&& H_FLAG) ||| cf

        operations.[0x2A] <- fun () ->
            let offset = x.read_address_pointing_by_pc
            register.[L] <- x.read_from_memory offset
            register.[H] <- x.read_from_memory (offset + 1us)

        operations.[0x2F] <- fun () ->
            register.[A] <- register.[A] ^^^ MASK_8
            register.[F] <- (register.[F] &&& SZP_FLAGS) ||| 
                    H_FLAG ||| 
                    (register.[A] &&& X53_FLAGS) ||| 
                    N_FLAG ||| 
                    (register.[F] &&& C_FLAG)

        operations.[0x31] <- fun () ->
            sp <- x.read_address_pointing_by_pc

        operations.[0x32] <- fun () ->
            x.write_to_memory x.read_address_pointing_by_pc register.[A]

        operations.[0x36] <- fun () ->
            x.write_to_memory (x.get_register16 H L) (x.read_from_memory pc)
            pc <- pc + 1us

        operations.[0x37] <- fun () ->
            register.[F] <- (register.[F] &&& SZP_FLAGS) ||| 
                            (register.[A] &&& X53_FLAGS) ||| C_FLAG

        operations.[0x3A] <- fun () ->
            let offset = x.read_address_pointing_by_pc
            register.[A] <- x.read_from_memory offset

        operations.[0x3F] <- fun () ->
            register.[F] <- (register.[F] &&& SZP_FLAGS) ||| 
                            (register.[A] &&& X53_FLAGS) ||| 
                            (if (register.[F] &&& C_FLAG) <> 0uy then H_FLAG else C_FLAG)

        operations.[0x76] <- fun () ->
            halt <- true
            pc <- pc - 1us

        operations.[0xC3] <- fun () ->
            pc <- x.read_address_pointing_by_pc

        operations.[0xC9] <- fun () ->
            pc <- x.pop_from_stack

        operations.[0xCB] <- fun () ->
            x.increment_memory_refresh_register
            let op = int x.read_next_instruction
            pc <- pc + 1us
            operations_bits.[op]()

        operations.[0xCD] <- fun () ->
            let offset = x.read_address_pointing_by_pc
            x.check_memory_contention_m (pc - 1us) 1
            x.push_to_stack pc
            pc <- offset

        operations.[0xD3] <- fun () ->
            let value = x.read_from_memory pc
            x.write_to_port (x.get_compound register.[A] value) register.[A]
            pc <- pc + 1us

        operations.[0xD9] <- fun () ->
            x.exchange_registers B BS
            x.exchange_registers C CS
            x.exchange_registers D DS
            x.exchange_registers E ES
            x.exchange_registers H HS
            x.exchange_registers L LS

        operations.[0xDB] <- fun () -> 
            register.[A] <- x.read_from_port (x.get_compound register.[A] (x.read_from_memory pc))
            pc <- pc + 1us

        operations.[0xDD] <- fun () ->
            x.increment_memory_refresh_register
            let op = int x.read_next_instruction
            pc <- pc + 1us
            operations_indexes.[op] IX

        operations.[0xE3] <- fun () -> 
            let curr_h = register.[H]
            let curr_l = register.[L]
            register.[L] <- x.read_from_memory sp
            register.[H] <- x.read_from_memory (sp + 1us)
            x.check_memory_contention_m (sp + 1us) 1
            x.write_to_memory sp curr_l
            x.write_to_memory (sp + 1us) curr_h
            x.check_memory_contention_m sp 2

        operations.[0xE9] <- fun () ->
            pc <- x.get_register16 H L

        operations.[0xEB] <- fun () ->
            x.exchange_registers D H
            x.exchange_registers E L

        operations.[0xED] <- fun () ->
            x.increment_memory_refresh_register
            let op = int x.read_next_instruction
            pc <- pc + 1us
            operations_extended.[op]()

        operations.[0xF3] <- fun () ->
            iff1 <- false
            iff2 <- false

        operations.[0xF9] <- fun () ->
            x.check_memory_contention_m ir 2
            sp <- x.get_register16 H L

        operations.[0xFB] <- fun () ->
            ei <- true
            iff1 <- true
            iff2 <- true

        operations.[0xFD] <- fun () ->
            x.increment_memory_refresh_register
            let op = int x.read_next_instruction
            pc <- pc + 1us
            operations_indexes.[op] IY

        [(0x01, [B; C]); (0x11, [D; E]); (0x21, [H; L])] |> List.iter 
            (fun c -> 
                match c with
                | (op, reg) ->
                    operations.[op] <- fun () ->
                        reg |> List.rev |> List.iter (fun reg_half ->
                            register.[reg_half] <- x.read_from_memory pc
                            pc <- pc + 1us))

        [(0x02, [B; C]); (0x12, [D; E])] |> List.iter 
            (fun c ->
                match c with
                | (op, [reg1; reg2]) ->
                    operations.[op] <- fun () -> 
                        x.write_to_memory (x.get_register16 reg1 reg2) register.[A]
                | (_, _) -> ()
            )

        [(0x33, +1); (0x3B, -1)] |> List.iter (fun c ->
            match c with
            | (op, offset) -> operations.[op] <- fun () ->
                             x.check_memory_contention_m ir 2
                             sp <- uint16 (int sp + offset))

        [
            (0x03, [B;C], +1)
            (0x13, [D;E], +1)
            (0x23, [H;L], +1)
            (0x0B, [B;C], -1)
            (0x1B, [D;E], -1)
            (0x2B, [H;L], -1)
        ] |> List.iter (fun c ->
            match c with
            | (op, [reg1; reg2], offset) -> 
                    operations.[op] <- fun () ->
                        x.check_memory_contention_m ir 2
                        let r = uint16 (int (x.get_register16 reg1 reg2) + offset)
                        register.[reg1] <- byte (r >>> 8)
                        register.[reg2] <- byte r
            | (_, _, _) -> ()
        )

        let get_ops_with_regs ops regs = List.zip ops regs
        let get_ops ops regs f = (get_ops_with_regs ops regs) |> List.map (fun (a, b) -> (a, b, f))
        let ops1 = get_ops (List.append [0x04 .. 0x08 .. 0x2C] [0x3C]) regs x.inc_reg_8
        let ops2 = get_ops (List.append [0x05 .. 0x08 .. 0x2D] [0x3D]) regs x.dec_reg_8
        List.append ops1 ops2 |> List.iter (fun c ->
            match c with
            | (op, reg, f) ->
                operations.[op] <- fun () -> f reg
        )

        get_ops_with_regs (List.append [0x06 .. 0x08 .. 0x2E] [0x3E]) regs |> List.iter (fun c ->
            match c with
            | (op, reg) ->
                operations.[op] <- fun () ->
                    register.[reg] <- x.read_from_memory pc
                    pc <- pc + 1us
        )

        [(0x09, [B;C]); (0x19, [D;E]); (0x29, [H;L]); (0x39, [])] |> List.iter (fun c ->
            match c with
            | (op, [reg1; reg2]) ->
                operations.[op] <- fun () ->
                    x.check_memory_contention_m ir 7
                    x.add_to_register16 H L (x.get_register16 reg1 reg2)
            | (op, _) ->
                operations.[op] <- fun () ->
                    x.check_memory_contention_m ir 7
                    x.add_to_register16 H L sp
        )

        [
            (0x20, (Z_FLAG, false))
            (0x30, (C_FLAG, false))
            (0x28, (Z_FLAG, true))
            (0x38, (C_FLAG, true))
        ] |> List.iter (fun c->
            match c with
            | (op, (flag, exp_flag_value)) ->
                operations.[op] <- fun () ->
                    let offset = x.get_signed_byte (x.read_from_memory pc)
                    let flag_value = register.[F] &&& flag > 0uy
                    if flag_value = exp_flag_value then
                        x.check_memory_contention_m pc 5
                        pc <- uint16 (int pc + offset + 1)
                    else
                        pc <- pc + 1us
        )

        [
            (0x34, x.inc_with_flags8)
            (0x35, x.dec_with_flags8)
        ] |> List.iter (fun c ->
            match c with
            | (op, f) ->
                operations.[op] <- fun () ->
                    let hl = x.get_register16 H L
                    let value = f (x.read_from_memory hl)
                    x.check_memory_contention_m hl 1
                    x.write_to_memory hl value
        )

        [
            (0x0A, [B;C])
            (0x1A, [D;E])
        ] |> List.iter (fun c ->
            match c with
            |(op, [reg1; reg2]) ->
                operations.[op] <- fun () ->
                    register.[A] <- x.read_from_memory (x.get_register16 reg1 reg2)
            |(_, _) -> ()
        )

        [
            ([0x40..0x47], B)
            ([0x48..0x4F], C)
            ([0x50..0x57], D)
            ([0x58..0x5F], E)
            ([0x60..0x67], H)
            ([0x68..0x6F], L)
            ([0x78..0x7F], A)
        ] |> List.iter (fun c ->
            match c with
            | (ops, dest_reg) ->
                List.zip ops [B;C;D;E;H;L;-1;A] |> List.iter (fun y ->
                    match y with
                    | (op, src_reg) ->
                        operations.[op] <- 
                         if src_reg <> -1 then
                            fun () -> register.[dest_reg] <- register.[src_reg]
                         else
                            fun () -> register.[dest_reg] <- x.read_from_memory (x.get_register16 H L)
                )
        )

        List.zip (List.append [0x70..0x75] [0x77]) regs |> List.iter (fun c ->
            match c with
            |(op, src_reg) ->
                operations.[op] <- fun () ->
                    x.write_to_memory (x.get_register16 H L) register.[src_reg]
        )

        [
            ([0x80..0x87], x.add_to_register)
            ([0x88..0x8F], x.add_to_register_with_carry)
            ([0x90..0x97], x.sub_from_register)
            ([0x98..0x9F], x.sub_from_register_with_carry)
            ([0xa0..0xA7], x.and_register)
            ([0xa8..0xAF], x.xor_register)
            ([0xb0..0xB7], x.or_register)
            ([0xb8..0xBF], x.cp_to_register)
        ] |> List.iter (fun c ->
            match c with
            | (ops, f) ->
               List.zip ops [B;C;D;E;H;L;-1;A] |> List.iter (fun y ->
                    match y with
                    | (op, src_reg) ->
                        operations.[op] <- 
                         if src_reg <> -1 then
                            fun () -> f A register.[src_reg]
                         else
                            fun () -> f A (x.read_from_memory (x.get_register16 H L))
               )
        )

        [
            (0xC0, (Z_FLAG, false))
            (0xD0, (C_FLAG, false))
            (0xE0, (P_FLAG, false))
            (0xF0, (S_FLAG, false))
            (0xC8, (Z_FLAG, true))
            (0xD8, (C_FLAG, true))
            (0xE8, (P_FLAG, true))
            (0xF8, (S_FLAG, true))
        ] |> List.iter (fun c ->
            match c with
            | (op, (flag, exp_flag_value)) ->
                operations.[op] <- fun () ->
                    x.check_memory_contention_m ir 1
                    let flag_value = register.[F] &&& flag > 0uy
                    if flag_value = exp_flag_value then
                        pc <- x.pop_from_stack
        )

        [
            (0xC1,[B;C])
            (0xD1,[D;E])
            (0xE1,[H;L])
            (0xF1,[A;F])
        ] |> List.iter (fun c ->
            match c with
            |(op, [reg1;reg2]) ->
                operations.[op] <- fun () ->
                    let v = x.pop_from_stack
                    register.[reg1] <- byte (v >>> 8)
                    register.[reg2] <- byte v
            | (_, _) -> ()
        )

        [
            (0xC2,(Z_FLAG, false))
            (0xD2,(C_FLAG, false))
            (0xE2,(P_FLAG, false))
            (0xF2,(S_FLAG, false))
            (0xCA,(Z_FLAG, true))
            (0xDA,(C_FLAG, true))
            (0xEA,(P_FLAG, true))
            (0xFA,(S_FLAG, true))
        ] |> List.iter (fun c ->
            match c with
            | (op, (flag, exp_flag_value)) ->
                operations.[op] <- fun () ->
                    let offset1 = x.read_from_memory pc
                    pc <- pc + 1us
                    let offset2 = x.read_from_memory pc
                    pc <- pc + 1us
                    let offset = x.get_compound offset2 offset1
                    let flag_value = register.[F] &&& flag > 0uy
                    if flag_value = exp_flag_value then
                        pc <- offset
        )

        [
          (0xC4,(Z_FLAG, false))
          (0xD4,(C_FLAG, false))
          (0xE4,(P_FLAG, false))
          (0xF4,(S_FLAG, false))
          (0xCC,(Z_FLAG, true))
          (0xDC,(C_FLAG, true))
          (0xEC,(P_FLAG, true))
          (0xFC,(S_FLAG, true))
        ] |> List.iter (fun c ->
            match c with
            | (op, (flag, exp_flag_value)) ->
                operations.[op] <- fun () ->
                    let offset1 = x.read_from_memory pc
                    let offset2 = x.read_from_memory (pc + 1us)
                    let offset = x.get_compound offset2 offset1
                    let flag_value = register.[F] &&& flag > 0uy
                    if flag_value = exp_flag_value then
                        pc <- pc + 1us
                        x.check_memory_contention_m pc 1
                        pc <- pc + 1us
                        x.push_to_stack pc
                        pc <- offset
                    else
                        pc <- pc + 2us
        )

        [
            (0xC5, [B;C])
            (0xD5, [D;E])
            (0xE5, [H;L])
            (0xF5, [A;F])
        ] |> List.iter (fun c ->
            match c with
            | (op, [reg1;reg2]) ->
                operations.[op] <- fun () ->
                    x.check_memory_contention_m ir 1
                    x.push_to_stack (x.get_register16 reg1 reg2)
            | (_, _) -> ()
        )

        [
            (0xC7,0x00us)
            (0xD7,0x10us)
            (0xE7,0x20us)
            (0xF7,0x30us)
            (0xCF,0x08us)
            (0xDF,0x18us)
            (0xEF,0x28us)
            (0xFF,0x38us)
        ] |> List.iter (fun c->
            match c with
            | (op, addr) ->
                operations.[op] <- fun () ->
                    x.check_memory_contention_m ir 1
                    x.push_to_stack pc
                    pc <- addr
        )

        [
            (0xC6, x.add_to_register)
            (0xD6, x.sub_from_register)
            (0xE6, x.and_register)
            (0xF6, x.or_register)
            (0xCE, x.add_to_register_with_carry)
            (0xDE, x.sub_from_register_with_carry)
            (0xEE, x.xor_register)
            (0xFE, x.cp_to_register)
        ] |> List.iter (fun c->
            match c with
            | (op, f) ->
                operations.[op] <- fun () ->
                    f A (x.read_from_memory pc)
                    pc <- pc + 1us
        )


    member x.exchange_registers reg1 reg2 =
        let tmp = register.[reg1]
        register.[reg1] <- register.[reg2]
        register.[reg2] <- tmp

    member x.get_signed_byte (v:byte) = int (sbyte v)

    member x.read_address_pointing_by_pc =
        let offset1 = x.read_from_memory pc
        pc <- pc + 1us
        let offset2 = x.read_from_memory pc
        pc <- pc + 1us
        x.get_compound offset2 offset1

    member x.sub8_carry a b = 
        let res = (int a - int b)
        (byte res, res < 0)

    member x.add8_carry a b =
        let res = (int a + int b)
        (byte res, res > 0xFF)

    member x.add_to_register reg v =
        let r = register.[reg]
        let res, carry = x.add8_carry r v
        let cf = if carry then C_FLAG else 0uy
        let lookup = ((r &&& 0x88uy) >>> 3) ||| ((v &&& 0x88uy) >>> 2) ||| ((res &&& 0x88uy) >>> 1)
        register.[F] <- sz53n_add.[(int)res] ||| cf
        register.[F] <- register.[F] ||| 
            if halfcarry_add_table.[(int)(lookup &&& 0x07uy)] then H_FLAG else 0uy
        register.[F] <- register.[F] ||| 
            if overflow_add_table.[(int)(lookup >>> 4)] then V_FLAG else 0uy
        register.[reg] <- res

    member x.sub_from_register reg v =
        let r = register.[reg]
        let res, carry = x.sub8_carry r v
        let cf = if carry then C_FLAG else 0uy
        register.[F] <- sz53n_sub.[int res] ||| cf
        let lookup = ((r &&& 0x88uy) >>> 3) ||| ((v &&& 0x88uy) >>> 2) ||| ((res &&& 0x88uy) >>> 1)
        register.[F] <- register.[F] ||| 
            if halfcarry_sub_table.[int (lookup &&& 0x07uy)] then H_FLAG else 0uy
        register.[F] <- register.[F] ||| 
            if overflow_sub_table.[int (lookup >>> 4)] then V_FLAG else 0uy
        register.[reg] <- res

    member x.get_register16 a b = x.get_compound register.[a] register.[b]

    member x.inc_with_flags8 value =
        let value = value + 1uy
        register.[F] <- sz53n_add.[int value] ||| (register.[F] &&& C_FLAG)
        register.[F] <- register.[F] ||| if (value &&& 0x0Fuy) = 0uy then H_FLAG else 0uy
        register.[F] <- register.[F] ||| if (value = 0x80uy) then V_FLAG else 0uy
        value

    member x.dec_with_flags8 value =
        let value = value - 1uy
        register.[F] <- sz53n_sub.[int value] ||| (register.[F] &&& C_FLAG)
        register.[F] <- register.[F] ||| if ((value &&& 0x0Fuy) = 0x0Fuy) then H_FLAG else 0uy
        register.[F] <- register.[F] ||| if (value = 0x7Fuy) then V_FLAG else 0uy
        value

    member x.inc_reg_8 reg =
        let value = register.[reg]
        let value = x.inc_with_flags8 value
        register.[reg] <- value

    member x.dec_reg_8 reg =
        let value = register.[reg]
        let value = x.dec_with_flags8 value
        register.[reg] <- value

    member x.add_to_register16 reg1 reg2 v =
        let a = x.get_register16 reg1 reg2
        let sum = x.add_to_register16_val a v
        register.[reg1] <- byte (sum >>> 8)
        register.[reg2] <- byte sum

    member x.add_to_register16_val a v =
        let value = int a + int v
        register.[F] <- (register.[F] &&& SZP_FLAGS) ||| 
                        ((byte (value >>> 8)) &&& X53_FLAGS) ||| 
                        (if value > 0xFFFF then C_FLAG else 0uy)
        let value = uint16 value;
        if (value &&& 0x0FFFus) < (a &&& 0x0FFFus) then
            register.[F] <- register.[F] ||| H_FLAG
        value

    member x.add_to_register_with_carry reg v =
        x.add_to_register reg (v + (if register.[F] &&& C_FLAG <> 0uy then 1uy else 0uy))

    member x.sub_from_register_with_carry reg v =
        x.sub_from_register reg (v + (if register.[F] &&& C_FLAG <> 0uy then 1uy else 0uy))

    member x.xor_register reg v =
        register.[reg] <- register.[reg] ^^^ v
        register.[F] <- sz53pn_add.[int register.[reg]]

    member x.or_register reg v =
        register.[reg] <- register.[reg] ||| v
        register.[F] <- sz53pn_add.[int register.[reg]]

    member x.and_register reg v =
        register.[reg] <- register.[reg] &&& v
        register.[F] <- sz53pn_add.[int register.[reg]] ||| H_FLAG

    member x.cp_to_register reg v =
        let r = register.[reg]
        let res, carry = x.sub8_carry r v
        let cf = if carry then C_FLAG else 0uy
        register.[F] <- (sz53n_sub.[int res] &&& SZHN_FLAGS) ||| 
                        (sz53n_add.[int v] &&& X53_FLAGS) ||| 
                        cf
        let lookup = ((r &&& 0x88uy) >>> 3) ||| 
                      ((v &&& 0x88uy) >>> 2) ||| 
                      ((res &&& 0x88uy) >>> 1)
        register.[F] <- register.[F] ||| 
                        (if halfcarry_sub_table.[int (lookup &&& 0x07uy)] then H_FLAG else 0uy)
        register.[F] <- register.[F] ||| 
                        (if overflow_sub_table.[int (lookup >>> 4)] then V_FLAG else 0uy)

    member x.initializeExtendedOperations =
        List.zip [0x40; 0x48; 0x50; 0x58; 0x60; 0x68; 0x78] regs |> List.iter (fun c ->
            match c with
            | (op, reg) ->
                operations_extended.[op] <- fun () ->
                    register.[reg] <- x.read_from_port (x.get_register16 B C)
                    register.[F] <- sz53pn_add.[int register.[B]] ||| (register.[F] &&& C_FLAG)
        )

        List.zip [0x41; 0x49; 0x51; 0x59; 0x61; 0x69; 0x79] regs |> List.iter (fun c ->
            match c with
            | (op, reg) ->
                operations_extended.[op] <- fun () ->
                    x.write_to_port (x.get_register16 B C) register.[reg]
        )

        [
            (0x42,[B;C])
            (0x52,[D;E])
            (0x62,[H;L])
            (0x72,[-1;-1])
        ] |> List.iter (fun c ->
            match c with
            |(op, [reg1;reg2]) ->
                operations_extended.[op] <- fun () ->
                    x.check_memory_contention_m ir 7
                    let hl_value = x.get_register16 H L
                    let reg_value = if reg1=(-1) then sp else x.get_register16 reg1 reg2
                    let res = (int hl_value - int reg_value - int (register.[F] &&& C_FLAG))
                    let cf = if res < 0 then C_FLAG else 0uy
                    let res = uint16 res
                    register.[H] <- byte (res >>> 8)
                    register.[L] <- byte res
                    register.[F] <- sz53n_sub.[int register.[H]] ||| cf
                    if res <> 0us then
                        register.[F] <- register.[F] &&& (Z_FLAG ^^^ 0xFFuy)

                    let lookup = ((hl_value &&& 0x8800us) >>> 11 ) ||| 
                                    (((reg_value) &&& 0x8800us) >>> 10 ) |||
                                    ((res &&& 0x8800us) >>>  9 )

                    register.[F] <- register.[F] ||| 
                        (if halfcarry_sub_table.[int (lookup &&& 0x07us)] then H_FLAG else 0uy)
                    register.[F] <- register.[F] ||| 
                        (if overflow_sub_table.[int (lookup >>> 4)] then V_FLAG else 0uy)
            |(_, _) -> ()
        )

        [
            (0x4A,[B;C])
            (0x5A,[D;E])
            (0x6A,[H;L])
            (0x7A,[-1;-1])
        ] |> List.iter (fun c ->
            match c with
            |(op, [reg1;reg2]) ->
                operations_extended.[op] <- fun () ->
                    x.check_memory_contention_m ir 7
                    let hl_value = x.get_register16 H L
                    let reg_value = if reg1=(-1) then sp else x.get_register16 reg1 reg2
                    let res = (int hl_value + int reg_value + int (register.[F] &&& C_FLAG))
                    let cf = if res > 0xFFFF then C_FLAG else 0uy
                    let res = uint16 res
                    register.[H] <- byte (res >>> 8)
                    register.[L] <- byte res
                    register.[F] <- sz53n_add.[int register.[H]] ||| cf
                    if res <> 0us then
                        register.[F] <- register.[F] &&& (Z_FLAG ^^^ 0xFFuy)

                    let lookup = ((hl_value &&& 0x8800us) >>> 11 ) ||| 
                                    (((reg_value) &&& 0x8800us) >>> 10 ) |||
                                    ((res &&& 0x8800us) >>>  9 )

                    register.[F] <- register.[F] ||| 
                        (if halfcarry_add_table.[int (lookup &&& 0x07us)] then H_FLAG else 0uy)
                    register.[F] <- register.[F] ||| 
                        (if overflow_add_table.[int (lookup >>> 4)] then V_FLAG else 0uy)
            |(_, _) -> ()
        )

        [
            (0x43,[B;C])
            (0x53,[D;E])
            (0x63,[H;L])
            (0x73,[-1;-1])
        ] |> List.iter (fun c ->
            match c with
            |(op, [reg1;reg2]) ->
                operations_extended.[op] <- fun () ->
                    let offset = x.read_address_pointing_by_pc
                    let value = if reg1 = (-1) then sp else x.get_register16 reg1 reg2
                    x.write_to_memory offset (byte value)
                    x.write_to_memory (offset + 1us) (byte (value >>> 8))
            |(_,_) -> ()
        )

        [
            (0x4B,[B;C])
            (0x5B,[D;E])
            (0x6B,[H;L])
            (0x7B,[-1;-1])
        ] |> List.iter (fun c ->
            match c with
            |(op, [reg1;reg2]) ->
                operations_extended.[op] <- fun () ->
                    let offset = x.read_address_pointing_by_pc
                    let val1 = x.read_from_memory offset
                    let val2 = x.read_from_memory (offset + 1us)
                    if reg1 = (-1) then
                        sp <- x.get_compound val2 val1
                    else
                        register.[reg1] <- val2
                        register.[reg2] <- val1
            |(_,_) -> ()
        )

        operations_extended.[0x44] <- fun () ->
            let v = register.[A]
            register.[A] <- 0uy
            x.sub_from_register A v

        [0x45; 0x55; 0x65; 0x75; 0x4D; 0x5D; 0x6D; 0x7D] |> List.iter (fun op ->
            operations_extended.[op] <- fun () ->
                iff1 <- iff2
                pc <- x.pop_from_stack
        )

        [
            (0x46,0)
            (0x56,1)
            (0x66,0)
            (0x76,1)
            (0x5E,2)
            (0x7E,2)
        ] |> List.iter (fun c ->
            match c with
            | (op, v) ->
                operations_extended.[op] <- fun () ->
                    im <- v
        )

        operations_extended.[0x47] <- fun () ->
            x.check_memory_contention_m ir 1
            ir <- ((uint16 register.[A]) <<< 8) ||| (ir &&& 0x00FFus)

        operations_extended.[0x4F] <- fun () ->
            x.check_memory_contention_m ir 1
            ir <- (ir &&& 0xFF00us) ||| ((uint16 register.[A]) &&& 0x00FFus)

        operations_extended.[0x57] <- fun () ->
            x.check_memory_contention_m ir 1
            register.[A] <- byte ((ir &&& 0xFF00us) >>> 8)
            register.[F] <- sz53n_add.[int register.[A]] ||| (register.[F] &&& C_FLAG)
            if iff2 then
                register.[F] <- register.[F] ||| P_FLAG

        operations_extended.[0x5F] <- fun () ->
            x.check_memory_contention_m ir 1
            register.[A] <- byte ir
            register.[F] <- sz53n_add.[int register.[A]] ||| (register.[F] &&& C_FLAG)
            if iff2 then
                register.[F] <- register.[F] ||| P_FLAG

        operations_extended.[0x67] <- fun () ->
            let a4 = register.[A] &&& 0x0Fuy
            let hl = x.get_register16 H L
            let hl8 = x.read_from_memory hl
            let hl4 = hl8 &&& 0x0Fuy
            register.[A] <- (register.[A] &&& 0xF0uy) ||| hl4
            x.check_memory_contention_m hl 4
            x.write_to_memory hl ((hl8 >>> 4) ||| (a4 <<< 4))
            register.[F] <- sz53pn_add.[int register.[A]] ||| (register.[F] &&& C_FLAG)

        operations_extended.[0x6F] <- fun () ->
            let a4 = register.[A] &&& 0x0Fuy
            let hl = x.get_register16 H L
            let hl8 = x.read_from_memory hl
            let hl4 = (hl8 &&& 0xF0uy) >>> 4
            register.[A] <- (register.[A] &&& 0xF0uy) ||| hl4
            x.check_memory_contention_m hl 4
            x.write_to_memory hl (((hl8 <<< 4) &&& MASK_8) ||| a4)
            register.[F] <- sz53pn_add.[int register.[A]] ||| (register.[F] &&& C_FLAG)

        [
            (0xA0, 1, 0)
            (0xB0, 1, -1)
            (0xA8, -1, 0)
            (0xB8, -1, 1)
        ] |> List.iter (fun c ->
            match c with
            |(op, incr, p) ->
                let f = fun () ->
                    let hl = x.get_register16 H L
                    let value = x.read_from_memory hl
                    let de = x.get_register16 D E
                    x.write_to_memory de value
                    x.check_memory_contention_m de 2
                    let hl = uint16 (int hl + incr)
                    register.[H] <- byte (hl >>> 8)
                    register.[L] <- byte hl

                    let de = uint16 (int de + incr)
                    register.[D] <- byte (de >>> 8)
                    register.[E] <- byte de

                    let bc = x.get_register16 B C
                    let bc = bc - 1us
                    register.[B] <- byte (bc >>> 8)
                    register.[C] <- byte bc

                    register.[F] <- (register.[F] &&& SZ_FLAGS) ||| 
                            (value &&& X3_FLAG) ||| 
                            (register.[F] &&& C_FLAG)
                    if value &&& N_FLAG <> 0uy then
                        register.[F] <- register.[F] ||| X5_FLAG
                    if bc <> 0us then
                        register.[F] <- register.[F] ||| P_FLAG
                operations_extended.[op] <- 
                    if p <> 0 then
                        fun () ->
                            f()
                            if (register.[F] &&& P_FLAG <> 0uy) then
                                pc <- (pc - 2us)
                                let de = x.get_register16 D E
                                x.check_memory_contention_m (uint16 (int de + p)) 5
                    else
                        fun () -> f()
        )

        [
            (0xA1, 1, 0)
            (0xB1, 1, -1)
            (0xA9, -1, 0)
            (0xB9, -1, 1)
        ] |> List.iter (fun c ->
            match c with
            |(op, incr, p) ->
                let f = fun () ->
                    let hl = x.get_register16 H L
                    let value = x.read_from_memory hl
                    let cf = register.[F] &&& C_FLAG
                    x.cp_to_register A value
                    x.check_memory_contention_m hl 5
                    let hl = uint16 (int hl + incr)
                    register.[H] <- byte (hl >>> 8)
                    register.[L] <- byte hl

                    let bc = x.get_register16 B C
                    let bc = bc - 1us
                    register.[B] <- byte (bc >>> 8)
                    register.[C] <- byte bc

                    let value = byte (int register.[A] - int value - 
                                    int (if register.[F] &&& H_FLAG <> 0uy 
                                         then 1uy else 0uy))
                    register.[F] <- (register.[F] &&& SZHN_FLAGS) ||| 
                                    (value &&& X3_FLAG) ||| cf
                    if value &&& N_FLAG <> 0uy then
                        register.[F] <- register.[F] ||| X5_FLAG
                    if bc <> 0us then
                        register.[F] <- register.[F] ||| P_FLAG
                operations_extended.[op] <- 
                    if p <> 0 then
                        fun () ->
                            f()
                            if (register.[F] &&& P_FLAG <> 0uy) && 
                               (register.[F] &&& Z_FLAG = 0uy) then
                                 pc <- (pc - 2us)
                                 let hl = x.get_register16 H L
                                 x.check_memory_contention_m (uint16 (int hl + p)) 5
                    else 
                        fun () -> f()
        )

        [
            (0xA2, 1, 0)
            (0xB2, 1, -1)
            (0xAA, -1, 0)
            (0xBA, -1, 1)
        ] |> List.iter (fun c ->
            match c with
            |(op, incr, p) ->
                let f = fun () ->
                    x.check_memory_contention_m ir 1
                    let bc = x.get_register16 B C
                    let value = x.read_from_port bc
                    let hl = x.get_register16 H L
                    x.write_to_memory hl value
                    register.[B] <- (register.[B] - 1uy)
                    let hl = uint16 (int hl + incr)
                    register.[H] <- byte (hl >>> 8)
                    register.[L] <- byte hl

                    register.[F] <- sz53pn_add.[int register.[B]]
                    if (value > 0x7Fuy) then
                        register.[F] <- register.[F] ||| N_FLAG
                    let value = value + (register.[C] + 1uy)
                    if (value > 0xFFuy) then
                        register.[F] <- register.[F] ||| H_FLAG ||| C_FLAG

                    if ((sz53pn_add.[int ((value &&& 0x07uy) ^^^ register.[B])] &&& P_FLAG) <> 0uy) then
                        register.[F] <- register.[F] ||| P_FLAG
                    else
                        register.[F] <- register.[F] &&& (P_FLAG ^^^ 0xFFuy)
                operations_extended.[op] <- 
                    if p <> 0 then
                        fun () ->
                            f()
                            if register.[B] <> 0uy then
                                pc <- (pc - 2us)
                                let hl = x.get_register16 H L
                                x.check_memory_contention_m (uint16 (int hl + p)) 5
                    else 
                        fun () -> f()
        )

        [
            (0xA3, 1, 0)
            (0xB3, 1, 1)
            (0xAB, -1, 0)
            (0xBB, -1, 1)
        ] |> List.iter (fun c ->
            match c with
            |(op, incr, p) ->
                let f = fun () ->
                    x.check_memory_contention_m ir 1
                    register.[B] <- register.[B] - 1uy
                    let hl = x.get_register16 H L
                    let value = x.read_from_memory hl
                    let bc = x.get_register16 B C
                    x.write_to_port bc value
                    let hl = uint16 (int hl + incr)
                    register.[H] <- byte (hl >>> 8)
                    register.[L] <- byte hl

                    register.[F] <- if value > 0x7Fuy then 
                                        sz53n_sub.[int register.[B]] 
                                        else sz53n_add.[int register.[B]]
                    if (register.[L] + value) > 0xFFuy then
                        register.[F] <- register.[F] ||| H_FLAG ||| C_FLAG
                    if (sz53pn_add.[int (((register.[L] + value) &&& 0x07uy) ^^^ 
                                       register.[B])] &&& P_FLAG) <> 0uy then
                            register.[F] <- register.[F] ||| P_FLAG

                operations_extended.[op] <- 
                    if p <> 0 then
                        fun () ->
                            f()
                            if register.[B] <> 0uy then
                                pc <- (pc - 2us)
                                let bc = x.get_register16 B C
                                x.check_memory_contention_m bc 5
                    else 
                        fun () -> f()

        )


    member x.initializeBitOperations = 
        [
            ([0x00..0x07], x.rotate_left_carry)
            ([0x08..0x0F], x.rotate_right_carry)
            ([0x10..0x17], x.rotate_left)
            ([0x18..0x1F], x.rotate_right)
            ([0x20..0x27], x.shift_left)
            ([0x28..0x2F], x.shift_right)
            ([0x38..0x3F], x.shift_right_logical)
        ] |> List.iter (fun c ->
            match c with
            | (ops, f) ->
               List.zip ops [B;C;D;E;H;L;-1;A] |> List.iter (fun y ->
                    match y with
                    | (op, reg) ->
                        operations_bits.[op] <- 
                         if reg <> -1 then
                            fun () -> register.[reg] <- f register.[reg]
                         else
                            fun () -> 
                                let hl_value = x.get_register16 H L
                                let res = f (x.read_from_memory hl_value)
                                x.check_memory_contention_m hl_value 1
                                x.write_to_memory hl_value res
               )
        )

        [
            ([0x40..0x47], x.test_bit, 0b00000001uy)
            ([0x48..0x4F], x.test_bit, 0b00000010uy)
            ([0x50..0x57], x.test_bit, 0b00000100uy)
            ([0x58..0x5F], x.test_bit, 0b00001000uy)
            ([0x60..0x67], x.test_bit, 0b00010000uy)
            ([0x68..0x6F], x.test_bit, 0b00100000uy)
            ([0x70..0x77], x.test_bit, 0b01000000uy)
            ([0x78..0x7F], x.test_bit, 0b10000000uy)
        ] |> List.iter (fun c ->
            match c with
            | (ops, f, mask) ->
                List.zip ops [B;C;D;E;H;L;-1;A] |> List.iter (fun y ->
                    match y with
                    | (op, reg) ->
                       operations_bits.[op] <- 
                         if reg <> -1 then
                            fun () -> f mask register.[reg]
                         else
                            fun () -> 
                                let hl_value = x.get_register16 H L
                                f mask (x.read_from_memory hl_value)
                                x.check_memory_contention_m hl_value 1
                )
        )

        [
            ([0x80..0x87], x.reset_bit, 0b11111110uy)
            ([0x88..0x8F], x.reset_bit, 0b11111101uy)
            ([0x90..0x97], x.reset_bit, 0b11111011uy)
            ([0x98..0x9F], x.reset_bit, 0b11110111uy)
            ([0xA0..0xA7], x.reset_bit, 0b11101111uy)
            ([0xA8..0xAF], x.reset_bit, 0b11011111uy)
            ([0xB0..0xB7], x.reset_bit, 0b10111111uy)
            ([0xB8..0xBF], x.reset_bit, 0b01111111uy)
            ([0xC0..0xC7], x.set_bit, 0b00000001uy)
            ([0xC8..0xCF], x.set_bit, 0b00000010uy)
            ([0xD0..0xD7], x.set_bit, 0b00000100uy)
            ([0xD8..0xDF], x.set_bit, 0b00001000uy)
            ([0xE0..0xE7], x.set_bit, 0b00010000uy)
            ([0xE8..0xEF], x.set_bit, 0b00100000uy)
            ([0xF0..0xF7], x.set_bit, 0b01000000uy)
            ([0xF8..0xFF], x.set_bit, 0b10000000uy)
        ] |> List.iter (fun c ->
            match c with
            | (ops, f, mask) ->
                List.zip ops [B;C;D;E;H;L;-1;A] |> List.iter (fun y ->
                    match y with
                    | (op, reg) ->
                       operations_bits.[op] <- 
                         if reg <> -1 then
                            fun () -> register.[reg] <- f mask register.[reg]
                         else
                            fun () -> 
                                let hl_value = x.get_register16 H L
                                let res = f mask (x.read_from_memory hl_value)
                                x.check_memory_contention_m hl_value 1
                                x.write_to_memory hl_value res
                )
        )


    member x.rotate_left_carry value =
        let cf = (value >>> 7)
        let value = (value <<< 1) ||| cf
        register.[F] <- sz53pn_add.[int value] ||| cf
        value

    member x.rotate_right_carry value =
        let cf = value &&& 0x01uy
        let value = (value >>> 1) ||| (cf <<< 7)
        register.[F] <- sz53pn_add.[int value] ||| cf
        value

    member x.rotate_left value =
        let cf = value >>> 7
        let value = (value <<< 1) ||| (register.[F] &&& C_FLAG)
        register.[F] <- sz53pn_add.[int value] ||| cf
        value

    member x.rotate_right value =
        let cf = value &&& 0x01uy
        let value = (value >>> 1) ||| ((register.[F] &&& C_FLAG) <<< 7)
        register.[F] <- sz53pn_add.[int value] ||| cf
        value

    member x.shift_left value =
        let cf = value >>> 7
        let value = (value <<< 1)
        register.[F] <- sz53pn_add.[int value] ||| cf
        value

    member x.shift_right value =
        let cf = value &&& 0x01uy
        let value = (value &&& 0x80uy) ||| (value >>> 1)
        register.[F] <- sz53pn_add.[int value] ||| cf
        value

    member x.shift_right_logical value =
        let cf = value &&& 0x01uy
        let value = value >>> 1
        register.[F] <- sz53pn_add.[int value] ||| cf
        value

    member x.test_bit (mask: byte) value =
        let zf = (mask &&& value) = 0uy
        register.[F] <- (sz53n_add.[int value] &&& (SZP_FLAGS ^^^ 0xFFuy)) ||| 
                            H_FLAG ||| (register.[F] &&& C_FLAG)
        if zf then
            register.[F] <- register.[F] ||| P_FLAG ||| Z_FLAG
        if (mask = 0x80uy) && (not zf) then
            register.[F] <- register.[F] ||| S_FLAG

    member x.reset_bit b value = value &&& b
    member x.set_bit b value = value ||| b

    member x.initializeIndexOperations =
        operations_indexes.[0x29] <- fun (ireg) ->
            x.check_memory_contention_m ir 7
            index_register.[ireg] <- x.add_to_register16_val index_register.[ireg] index_register.[ireg]

        operations_indexes.[0x39] <- fun (ireg) ->
            x.check_memory_contention_m ir 7
            index_register.[ireg] <- x.add_to_register16_val index_register.[ireg] sp

        [
            (0x09, [B;C])
            (0x19, [D;E])
        ] |> List.iter (fun c ->
            match c with
            | (op, [reg1;reg2]) ->
                operations_indexes.[op] <- fun (ireg) ->
                    x.check_memory_contention_m ir 7
                    index_register.[ireg] <- 
                        x.add_to_register16_val index_register.[ireg] (x.get_register16 reg1 reg2)
            | (_, _) -> ()
        )

        let f = fun ireg g ->
            let offset = x.get_signed_byte (x.read_from_memory pc)
            x.check_memory_contention_m pc 5
            let address = uint16 (int index_register.[ireg] + offset)
            g address
            pc <- pc + 1us

        [
            (0x46, B, 0);(0x4E, C, 0);(0x56, D, 0);(0x5E, E, 0);(0x66, H, 0);(0x6E, L, 0);(0x7E, A, 0);
            (0x70, B, 1);(0x71, C, 1);(0x72, D, 1);(0x73, E, 1);(0x74, H, 1);(0x75, L, 1);(0x77, A, 1);
        ] |> List.iter (fun c ->
            match c with
            | (op, reg, p) ->
                operations_indexes.[op] <- 
                    if p = 0 then 
                        fun (ireg) -> f ireg (fun (address) -> register.[reg] <- x.read_from_memory address)
                    else 
                        fun (ireg) -> f ireg (fun (address) -> x.write_to_memory address register.[reg])
        )

        [
            (0x86, x.add_to_register)
            (0x96, x.sub_from_register)
            (0xA6, x.and_register)
            (0xB6, x.or_register)
            (0x8E, x.add_to_register_with_carry)
            (0x9E, x.sub_from_register_with_carry)
            (0xAE, x.xor_register)
            (0xBE, x.cp_to_register)
        ] |> List.iter (fun c ->
            match c with
            | (op, g) ->
                operations_indexes.[op] <- fun (ireg) ->
                   f ireg (fun address -> g A (x.read_from_memory address))
        )

        operations_indexes.[0x21] <- fun ireg -> index_register.[ireg] <- x.read_address_pointing_by_pc

        operations_indexes.[0x22] <- fun ireg ->
            let address = x.read_address_pointing_by_pc
            x.write_to_memory address (byte index_register.[ireg])
            x.write_to_memory (address + 1us) (byte (index_register.[ireg] >>> 8))

        [
            (0x23, 1)
            (0x2B, -1)
        ] |> List.iter (fun c ->
            match c with
            | (op, p) ->
                operations_indexes.[op] <- fun (ireg) ->
                   x.check_memory_contention_m ir 2
                   index_register.[ireg] <- uint16 (int index_register.[ireg] + p)
        )

        operations_indexes.[0x2A] <- fun ireg ->
            let address = x.read_address_pointing_by_pc
            let v1 = x.read_from_memory address
            let v2 = x.read_from_memory (address + 1us)
            index_register.[ireg] <- x.get_compound v2 v1

        [
            (0x34, x.inc_with_flags8)
            (0x35, x.dec_with_flags8)
        ] |> List.iter (fun c ->
            match c with
            | (op, f) ->
                operations_indexes.[op] <- fun (ireg) ->
                    let offset = x.get_signed_byte (x.read_from_memory pc)
                    x.check_memory_contention_m pc 5
                    let address = uint16 (int index_register.[ireg] + offset)
                    let value = x.read_from_memory address
                    x.check_memory_contention_m address 1
                    x.write_to_memory address (f value)
                    pc <- pc + 1us
        )

        operations_indexes.[0x36] <- fun ireg ->
            let offset = x.get_signed_byte (x.read_from_memory pc)
            let address = uint16 (int index_register.[ireg] + offset)
            pc <- pc + 1us
            let value = x.read_from_memory pc
            x.check_memory_contention_m pc 2
            x.write_to_memory address value
            pc <- pc + 1us

        operations_indexes.[0xE1] <- fun ireg ->
            index_register.[ireg] <- x.pop_from_stack

        operations_indexes.[0xE5] <- fun ireg ->
            x.check_memory_contention_m ir 1
            x.push_to_stack index_register.[ireg]

        operations_indexes.[0xE9] <- fun ireg ->
            pc <- index_register.[ireg]

        operations_indexes.[0xF9] <- fun ireg ->
            x.check_memory_contention_m ir 2
            sp <- index_register.[ireg]

        operations_indexes.[0xE3] <- fun ireg ->
            let value = index_register.[ireg]
            let v1 = x.read_from_memory sp
            let v2 = x.read_from_memory (sp + 1us)
            index_register.[ireg] <- x.get_compound v2 v1
            x.check_memory_contention_m (sp + 1us) 1
            x.write_to_memory (sp + 1us) (byte (value >>> 8))
            x.write_to_memory sp (byte value)
            x.check_memory_contention_m sp 2

        operations_indexes.[0xCB] <- fun ireg ->
            let offset = x.get_signed_byte (x.read_from_memory pc)
            let address = uint16 (int index_register.[ireg] + offset)
            pc <- pc + 1us
            let next_op = x.read_from_memory pc
            x.check_memory_contention_m pc 2
            pc <- pc + 1us
            operations_indexes_bits.[int next_op] address (x.read_from_memory address)


    member x.initializeIndexBitOperations = 
        [
            (0x06, x.rotate_left_carry)
            (0x0E, x.rotate_right_carry)
            (0x16, x.rotate_left)
            (0x1E, x.rotate_right)
            (0x26, x.shift_left)
            (0x2E, x.shift_right)
            (0x3E, x.shift_right_logical)
        ] |> List.iter (fun c ->
            match c with
            | (op, f) ->
                operations_indexes_bits.[op] <- fun address value ->
                    let res = f value
                    x.check_memory_contention_m address 1
                    x.write_to_memory address res
        )

        [
            (0x46,0b00000001uy)
            (0x4E,0b00000010uy)
            (0x56,0b00000100uy)
            (0x5E,0b00001000uy)
            (0x66,0b00010000uy)
            (0x6E,0b00100000uy)
            (0x76,0b01000000uy)
            (0x7E,0b10000000uy)
        ] |> List.iter (fun c ->
            match c with
            | (op, mask) ->
                operations_indexes_bits.[op] <- fun address value ->
                    x.test_bit mask value
                    x.check_memory_contention_m address 1
        )

        [
            (0x86, x.reset_bit,0b11111110uy)
            (0x8E, x.reset_bit,0b11111101uy)
            (0x96, x.reset_bit,0b11111011uy)
            (0x9E, x.reset_bit,0b11110111uy)
            (0xA6, x.reset_bit,0b11101111uy)
            (0xAE, x.reset_bit,0b11011111uy)
            (0xB6, x.reset_bit,0b10111111uy)
            (0xBE, x.reset_bit,0b01111111uy)
            (0xC6, x.set_bit,0b00000001uy)
            (0xCE, x.set_bit,0b00000010uy)
            (0xD6, x.set_bit,0b00000100uy)
            (0xDE, x.set_bit,0b00001000uy)
            (0xE6, x.set_bit,0b00010000uy)
            (0xEE, x.set_bit,0b00100000uy)
            (0xF6, x.set_bit,0b01000000uy)
            (0xFE, x.set_bit,0b10000000uy)
        ] |> List.iter (fun c ->
            match c with
            | (op, f, mask) ->
                operations_indexes_bits.[op] <- fun address value ->
                    let v = f mask value
                    x.check_memory_contention_m address 1
                    x.write_to_memory address v
        )


end
type Machine(display: IDisplay, keyboard: IPort) as this = class
    let mutable processor = new Z80(keyboard)

    let measure_execution_time f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        f()
        timer.Stop()
        timer.ElapsedMilliseconds

    let machine = async {
            processor <- new Z80(keyboard)
            processor.loadRom(
                    if (Environment.GetCommandLineArgs().Length > 1) then 
                        Environment.GetCommandLineArgs().[1] 
                    else 
                        "48k.rom"
                )
            let! token = Async.CancellationToken
            while not token.IsCancellationRequested do
                let execution_time = 
                    measure_execution_time 
                        (fun () -> display.draw processor.execute_frame processor.Frames)
                let difference = 20L - execution_time
                if difference > 0L then
                    Thread.Sleep(int difference)
        }
    let mutable cancellationToken = new System.Threading.CancellationTokenSource()

    do
        this.Reset

    member x.Reset = 
        cancellationToken.Cancel()
        cancellationToken <- new System.Threading.CancellationTokenSource()
        Async.StartAsTask(machine, cancellationToken=cancellationToken.Token) |> ignore

    member x.Stop =
       cancellationToken.Cancel() 
end

type Z80GUI() as this = class
    inherit Form(Width = 256 + 70, Height = 192 + 120, Text = "Z80")

    let formSize = new Size(Width = 256 + 70, Height = 192 + 120)
    let mainMenu = new MainMenu()
    let mnuFile = new MenuItem()
    let mnuSystem = new MenuItem()
    let mnuFileClose = new MenuItem()
    let mnuSystemReset = new MenuItem()

    let keyboard = new Keyboard()
    let display = new Display(new VideoMemoryProcessor()) 
    let machine = new Machine(display, keyboard)

    do
        this.MaximumSize <- formSize
        this.MinimumSize <- formSize

        this.BackColor <- Color.FromArgb(0xFF, 0xC0, 0xC0, 0xC0)
        mnuFile.Text <- "&File"
        mnuSystem.Text <- "&System"
        mnuFileClose.Text <- "&Close"
        mnuSystemReset.Text <- "&Reset"
        mnuSystemReset.Click.Add(fun e -> 
            let r = MessageBox.Show(
                        text="Are you sure you want to reset the machine?", 
                        caption="Reset", 
                        buttons=MessageBoxButtons.YesNo) 
            if r = DialogResult.Yes then machine.Reset)
        mnuFileClose.Click.Add(fun e -> machine.Stop; this.Close())

        mnuFile.MenuItems.AddRange([| mnuFileClose; |])
        mnuSystem.MenuItems.AddRange([| mnuSystemReset; |])

        mainMenu.MenuItems.AddRange([| mnuFile; mnuSystem |])
        this.Menu <- mainMenu

        this.KeyDown.Add(fun e -> (keyboard :> IKeyboard).KeyPressed e.KeyCode)
        this.KeyUp.Add(fun e -> (keyboard :> IKeyboard).KeyReleased e.KeyCode)
        display.Location <- new Point(25, 25)
        display.Dock <- DockStyle.None
        display.Size <- new Size(256, 192)
        this.Controls.Add(display)
end 

