namespace FSpectrum
open System
open System.Windows.Forms

module Spectrum =
   [<EntryPoint>]
   do Application.Run(new Z80GUI())


