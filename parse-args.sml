local
 fun splitWith d = String.tokens (fn(c) => c = d)

 fun makeGetArg args =
   let
     val argsPair = List.map (( fn (x::y::nil) => (x, y) | _ => ("", "") ) o splitWith #"=" o List.hd o splitWith #"-") args
   in
     fn(key, default) =>
       let
         fun lookIn []          = default
           | lookIn ((k, v)::t) = if k = key then v else lookIn t
       in
         lookIn argsPair
       end
   end
  
  val progName = CommandLine.name()

  val help = "Using example:\n" ^
             progName ^ " --nodes=10.1.1.2:6380,10.1.1.3:6380,...\n\n" ^
             "Others parameters:\n--host=10.1.1.1\n--port=6379\n" ^
             "--timeout=300 (0 - disable timeout)\n" ^
             "--N=0 (set > 0 to use fork (MLton) or threads (PolyML))\n" ^
             "--SO_REUSEPORT=0\n"

  fun printError msg = print (msg ^ "\n" ^ help)
 
in
 fun getParam args =
   let
     val getArg = makeGetArg args
     
     val host    = getArg("host", "0.0.0.0")
     val port    = getArg("port", "6379")
     val nodes   = getArg("nodes", "")
     val timeout = getArg("timeout", "300")
     val N       = getArg("N", "0")
     val SO_REUSEPORT = getArg("SO_REUSEPORT", "0")

   in
     if nodes = "" then (printError "Parameter 'nodes' is required.\n"; NONE) else
     case Int.fromString port         of NONE => (printError "Parameter 'port' must be int.\n"; NONE) | SOME port =>
     case LargeInt.fromString timeout of NONE => (printError "Parameter 'timeout' must be int.\n"; NONE) | SOME timeout =>
     case Int.fromString N            of NONE => (printError "Parameter 'N' must be int.\n"; NONE) | SOME N =>
     case Int.fromString SO_REUSEPORT of NONE => (printError "Parameter 'SO_REUSEPORT' must be int.\n"; NONE) | SOME SO_REUSEPORT =>
     let
       val nodes = List.filter (fn(x,y) => (x <> "") andalso (y <> "")) 
                   (List.map (( fn (x::y::nil) => (x, y) | _ => ("", "") ) o splitWith #":") (splitWith #"," nodes))

      fun nodesPortToInt []          y = SOME (List.rev y)
        | nodesPortToInt ((h,p)::xs) y = case Int.fromString p of NONE => NONE | SOME p => nodesPortToInt xs ((h,p)::y)

     in
       case nodesPortToInt nodes [] of NONE => NONE | SOME nodes =>
       SOME (host, port, nodes, timeout, N, SO_REUSEPORT)
     end
   end
end


(*
val args = [
  "--host=127.0.0.5",
  "--port=8080",
  "--nodes=127.0.0.5:8081,127.0.0.5:8082,127.0.0.5:8083,127.0.0.5:8084",
  "--timeout=600",
  "--N=5"
]

val _ = case getParam args of NONE => () | SOME (host, port, nodes, timeout, N) => print "OK\n"
val _ = print "Parse args. The End\n"
*)
