fun main_wrapper f x = (
  MLton.Signal.setHandler(Posix.Signal.pipe, MLton.Signal.Handler.ignore);
  (*
  MLton.Signal.setHandler(Posix.Signal.term, MLton.Signal.Handler.simple (fn () => (print "TERM\n")));
  MLton.Signal.setHandler(Posix.Signal.alrm, MLton.Signal.Handler.simple (fn () => (print "ALRM\n")));
  *)
  f x
)



local
  val crc32_zlib_ffi = _import "crc32" : Word32.word * string * Word32.word -> Word32.word;
in
  fun crc32_zlib (s:string) : Word32.word = crc32_zlib_ffi (0w0, s, Word32.fromInt(String.size s))
end


exception Socket of string

local
  val setsockopt_REUSEPORT_ffi = _import "setsockopt_REUSEPORT": int -> int;
in
  fun setsockopt_REUSEPORT fd =
    if setsockopt_REUSEPORT_ffi fd = ~1
    then raise Socket "Cannot set SO_REUSEPORT option on socket"
    else ()
end


local
  val pidToString = LargeInt.toString o SysWord.toLargeInt o Posix.Process.pidToWord
  fun myPidAsString () = pidToString (Posix.ProcEnv.getpid ())

  fun doFork 0 f x = ()
    | doFork n f x = 
        case Posix.Process.fork () of
             NONE => (
                print ("I am child, my PID is " ^ ( myPidAsString () ) ^ ".\n");
                f x;
                Posix.Process.exit 0w0
                )
           | SOME pid => doFork (n -1) f x

  fun waitN 0 = ()
    | waitN n = ( Posix.Process.wait (); waitN (n - 1) )

in
  fun runWithN n f x =
    if n > 1
    then
    (
      print ("My PID is " ^ ( myPidAsString () ) ^ ".\n");
      doFork n f x;
      waitN n
    )
    else f x
end
