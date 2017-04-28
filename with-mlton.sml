fun main_wrapper f x = (
  MLton.Signal.setHandler(Posix.Signal.pipe, MLton.Signal.Handler.ignore);
  (*
  MLton.Signal.setHandler(Posix.Signal.term, MLton.Signal.Handler.simple (fn () => (print "TERM Signal got.\n")));
  MLton.Signal.setHandler(Posix.Signal.alrm, MLton.Signal.Handler.simple (fn () => (print "ALRM Signal got.\n")));
  MLton.Signal.setHandler(Posix.Signal.chld, MLton.Signal.Handler.simple (fn () => (print "CHLD Signal got.\n")));
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

  val main_pid = Posix.ProcEnv.getpid ()

  fun setHandlerForTermSignal () = (   (* send signal to group *)
      MLton.Signal.setHandler(Posix.Signal.term, MLton.Signal.Handler.simple (fn () => (
        (* print "TERM Signal got.\n"; *)
        (* print ( (Int.toString (SysWord.toInt (Posix.Signal.toWord Posix.Signal.term))) ^ " Signal got.\n"); *)
        if main_pid = Posix.ProcEnv.getpid ()
        then Posix.Process.kill (Posix.Process.K_GROUP (Posix.ProcEnv.getpid ()), Posix.Signal.term)
        else () ;
        Posix.Process.exit 0w0
      )))
    )

  fun doFork 0 f x = ()
    | doFork n f x =
        case Posix.Process.fork () of
             NONE => (
                print ("I am child, my PID is " ^ ( myPidAsString () ) ^ ".\n");
                f x;
                Posix.Process.exit 0w0
                )
           | SOME pid => doFork (n - 1) f x

  fun wait f x =  (
      Posix.Process.wait ();
      doFork 1 f x;
      wait f x
    )


in
  fun runWithN n f x =
    if n > 1
    then
    (
      setHandlerForTermSignal ();
      print ("My PID is " ^ ( myPidAsString () ) ^ ".\n");
      doFork n f x;
      wait f x
    )
    else f x
end
