open Ev
structure H = HashArrayInt

datatype RCmd = RCmd of string * int list (* Имя команды и на какие сервера послали конкретные данные. *)

datatype ServerReadMode = BaseServerReadMode   (* Со всем указанных серверов читаются ответы *)
                        | MultiServerReadMode  (* С указанного сервера, потом с друго, в порядке указанной в RCmd типе *)
                        | OneServerReadMode    (* С указанного сервера *)
                        | FreeServerReadMode   (* Как читается, так и возвращается. Для команды KEYS *)
                        (* Сначала включен BaseServerReadMode, затем когда от всех, с которых нужно, прочитали размер multi_bulk ответа,
                           переходим в один из режимов: MultiServerReadMode, OneServerReadMode или FreeServerReadMode, в последний - когда сказано,
                           что ответ со всех серверов, что обозначается пустым списком серверов. *)

val send_size = 32 * 1024
val recv_size = 32 * 1024


local
  fun for_over_fold fold f l = fold (fn (v,_) => f v) () l
in
  fun for f l = for_over_fold foldl f l
  fun vector_for f l = for_over_fold Vector.foldl f l
end


val sockToEvFD : ('a, 'b) Socket.sock -> int = fn sock => (SysWord.toInt o Posix.FileSys.fdToWord o Option.valOf o Posix.FileSys.iodToFD o Socket.ioDesc) sock


fun key_tag "" = ""
  | key_tag key =
     let
       val length = String.size key
       fun search_brace ~1 = NONE
         | search_brace i  = if String.sub (key, i) = #"{" then (SOME i) else search_brace (i - 1)
     in
       if String.sub (key, length - 1) = #"}"
       then
         case search_brace (length - 2) of
              SOME i => String.substring (key, i + 1, length - i - 2)
            | NONE   => key
       else key
     end


fun key2server key s_count = Word32.toInt (Word32.mod ((crc32_zlib (key_tag key)), Word32.fromInt s_count))


(* Remove x from a list *)
fun remove (_, []) = []
  | remove (x, l as y::ys) = if x = y then remove (x, ys) else y::remove (x, ys)


(* Remove duplicates from a list *)
fun isolate [] = []
  | isolate (x::xs) = x::isolate(remove(x,xs))


local
  val arr = Word8Array.array(recv_size, 0w0)
in
  fun recvString sock =
    let
      fun recvStringNewDo sock =
        let
          val recvd = Socket.recvArr(sock, Word8ArraySlice.full arr)
          val arr_slice = Word8ArraySlice.slice(arr, 0, SOME recvd)
        in
          Byte.unpackString arr_slice
        end
    in
      recvStringNewDo sock handle (OS.SysErr _) => ""
    end
end


fun printLog msg = print ((Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))) ^ "\t" ^ msg ^ "\n")


fun client_stream ev c_info s_info set_cmd clean servers_stream_up c_send_and_flush =
  let
    val (c_sock, c_fd) = c_info
    val c_actived = ref false
    val c_buf     = ref ""

    val all_channel_size = ref 0
    val all_channel_size_max = recv_size
    val s_count = Vector.length s_info
    val all_s_addrs = List.tabulate (s_count, fn i => i)
    fun s_info_to_s_data (sock, fd) = { sock = sock, fd = fd, actived = ref false, buf = ref "", channel = ref ListBuf.empty }
    val s_data = Vector.map s_info_to_s_data s_info

    val in_closed = ref false

    fun to_channel s_addr cs =
      let
        val data = Vector.sub (s_data, s_addr)
        val channel = #channel data
        val cs_size = List.foldl (fn (s, size) => size + String.size s) 0 cs
      in
        channel := ListBuf.add (!channel) cs;
        all_channel_size := !all_channel_size + cs_size
      end

    fun to_channel_all cs = for (fn s_addr => to_channel s_addr cs) all_s_addrs

    fun c_enable  () = if !c_actived = false then ( evModify ev [evAdd (c_fd, evRead, c_cb)] ; c_actived := true  ) else ()
    and c_disable () = if !c_actived = true  then ( evModify ev [evDelete (c_fd, evRead)] ;    c_actived := false ) else ()


    (* s_enable_all - пробегает по всем серверам, выбирает только не активные, и для тех, у кого есть в канале данные активирует запись. *)
    and s_enable_all () =
      let
        fun s_enable data = if !(#actived data) then () else if ListBuf.size(!(#channel data)) = 0 then () else
          ( evModify ev [evAdd ((#fd data), evWrite, (s_cb data))] ; (#actived data) := true )
      in
        vector_for s_enable s_data
      end

    and s_disable data = if !(#actived data) then ( evModify ev [evDelete ((#fd data), evWrite)] ; (#actived data) := false ) else ()

    (* Когда в s буфере нет данных *)
    and s_buf_empty data =
      if !all_channel_size = 0
      then (
        s_disable data; c_enable ();
        if (!c_buf) = "" then () else c_doit ""
      )
      else ()

    and c_cb (fd, _) =
      let
        val s = recvString c_sock
      in
        if s = ""
        then clean () (* s вернул пустую строку - закрылся *)
        else c_doit s
      end

     (* Возврашает данные из канала. *)
    and s_from_channel data =
      if ListBuf.size(!(#channel data)) = 0 then "" else
      let
        val s = ListBuf.toString (!(#channel data))
        val _ = all_channel_size := !all_channel_size - ListBuf.size(!(#channel data))
        val _ = (#channel data)  := ListBuf.empty
      in
        s
      end

    (* Возвращаем out буфер или из channel *)
    and s_get_buf data =
      let
        val buf = !(#buf data)
      in
        if buf <> ""
        then buf
        else s_from_channel data
      end

    (* Записать данные в s, а то, что осталось, поместить в buf *)
    and send_out_bug data buf =
      let
        val strToSlc = Word8VectorSlice.full o Byte.stringToBytes
        val cnt = Socket.sendVec ((#sock data), (strToSlc buf)) handle (OS.SysErr _) => 0
      in
        if cnt > 0
        then (#buf data) := String.extract (buf, cnt, NONE)
        else ()
      end

    and s_cb data (fd, _) =
      let
         val buf = s_get_buf data
      in
        if buf = ""
        then s_buf_empty data
        else send_out_bug data buf
      end

    (*
    Вызываем парсер, его успешный результат передаем в to_channel.
    Если to_channel сказал, что нужно уже писать, так как заполнен, то активируем out и останавливаме in.
    Если парсер сказал, что нужно писать, вернув пустой хвост, то активируем out.
    *)
    and c_doit s =
      let
        fun c_parse buf =
        case multi_bulk_parser (buf) of
            Done (SOME (ca as (SOME c)::args), t) => (
                let
                  val cmd = String.map Char.toUpper c
                in
                  (* print (cmd ^ " cmd to\n"); *) (* ToDo *)
                  (
                  case cmdType cmd of
                       SOME CmdToAll         => (* На все сервера *)
                            let
                              val _ = set_cmd (RCmd (cmd, all_s_addrs))
                              val cs = cmd2stream ca
                            in
                              to_channel_all cs;
                              servers_stream_up ()
                            end
                     | SOME CmdToOne         => (* На конкретные сервер *)
                            let
                              val arg = List.hd args
                              val s_addr = key2server (valOf arg) s_count
                              val _ = set_cmd (RCmd (cmd, [s_addr]))
                              val cs = cmd2stream ca
                            in
                              to_channel s_addr cs;
                              servers_stream_up ()
                            end
                     | SOME CmdToMany        => (* На множество серверов. CMD key1 ... keyN *)
                            let
                              val arg_and_s_addr = map (fn arg => (arg, (key2server (valOf arg) s_count))) args
                              val(_, s_addrs) = ListPair.unzip arg_and_s_addr
                              val uniq_s_addrs = isolate s_addrs
                              val _ = set_cmd (RCmd (cmd, s_addrs))
                              val _ = for ( fn s_addr =>
                                  let
                                    val (args_, _) = ListPair.unzip
                                      (List.filter ( fn (arg, s_addr_) => s_addr_ = s_addr ) arg_and_s_addr)
                                    val cs = cmd2stream ((SOME cmd)::args_)
                                  in
                                    to_channel s_addr cs
                                  end
                               ) uniq_s_addrs
                            in
                              servers_stream_up ()
                            end
                     | SOME CmdToManyValues  => (* На множество серверов. CMD key1 value1 ... keyN valueN *)
                            let
                              fun to_pair []        = []
                                | to_pair (a::[])   = []
                                | to_pair (a::b::l) = (a,b)::(to_pair l)

                              val arg_and_s_addr = map (fn (k, v) => ((k, v), (key2server (valOf k) s_count))) (to_pair args)
                              val(_, s_addrs) = ListPair.unzip arg_and_s_addr
                              val uniq_s_addrs = isolate s_addrs
                              val _ = set_cmd (RCmd (cmd, s_addrs))
                              val _ = for ( fn s_addr =>
                                  let
                                    val args_ = List.concat (List.map ( fn ((k,v),_) => [k,v] )
                                      (List.filter ( fn (arg, s_addr_) => s_addr_ = s_addr ) arg_and_s_addr)
                                      )
                                    val cs = cmd2stream ((SOME cmd)::args_)
                                  in
                                    to_channel s_addr cs
                                  end
                               ) uniq_s_addrs
                            in
                              servers_stream_up ()
                            end
                     | SOME CmdToManyTimeout => (* На множество серверов. CMD key1 ... keyN timeout (блокирующие команды)*)
                            let
                              val timeout = List.last args
                              val keys = List.take(args, List.length args - 1)
                              val arg_and_s_addr = map (fn arg => (arg, (key2server (valOf arg) s_count))) keys
                              val(_, s_addrs) = ListPair.unzip arg_and_s_addr
                              val uniq_s_addrs = isolate s_addrs
                              val _ = set_cmd (RCmd (cmd, s_addrs))
                            in
                              if List.length uniq_s_addrs <> 1
                              then c_send_and_flush ["-ERR Keys of the '", cmd, "' command should be on one node; use key tags\r\n"] else
                              for ( fn s_addr =>
                                  let
                                    val (args_, _) = ListPair.unzip
                                      (List.filter ( fn (arg, s_addr_) => s_addr_ = s_addr ) arg_and_s_addr)
                                    val cs = cmd2stream (((SOME cmd)::args_) @ [timeout])
                                  in
                                    to_channel s_addr cs
                                  end
                                ) uniq_s_addrs;
                              servers_stream_up ()
                            end
                     | NONE => c_send_and_flush ["-ERR unsupported command '" ^ cmd ^ "'\r\n"]
                  );
                  if !all_channel_size >= all_channel_size_max
                  then ( s_enable_all (); c_disable (); t )
                  else
                  if Substring.isEmpty t
                  then (s_enable_all (); t)
                  else c_parse t
                end
              )
          | Done _  => (c_send_and_flush ["-ERR unified protocol error\r\n"]; Substring.full "")
          | Partial => buf
          | Fail    => (c_send_and_flush ["-ERR unified protocol error\r\n"]; Substring.full "")

        (* val buf = (!c_buf) ^ s *)
        val buf = String.^((!c_buf), s)
      in
        c_buf := Substring.string (c_parse (Substring.full buf))
      end
  in
    c_enable ()
  end



fun servers_stream ev c_info s_info get_cmd clean set_time_last_activity =
  let
    fun s_info_to_s_data (sock, fd) = { sock = sock, fd = fd, actived = ref false, buf = ref (Substring.full ""),
        base_reply  = ref NONE, (* Используется только для BaseServerReadMode *)
        do_item_cnt = ref 0     (* Сколько элементов ответа осталось прочитать и обработать. Для multi_bulk ответов, для FreeServerReadMode *)
        }
    val s_data = Vector.map s_info_to_s_data s_info

    val (c_sock, c_fd) = c_info
    val c_actived = ref false
    val c_buf     = ref ""
    val c_channel = ref ListBuf.empty

    val read_mode = ref BaseServerReadMode

    val current_cmd : (RCmd * int list) option ref = ref NONE
    fun clean_current_cmd () = (read_mode := BaseServerReadMode; current_cmd := NONE)

    val multi_read_s_addrs: int list ref = ref [] (* Используется только для MultiServerReadMode, откуда осталось еще прочитать *)
    val one_read:        (int * int) ref = ref (0, 0) (* Сколько еще прочитать и откуда, для OneServerReadMode *)
    val free_size_todo:     int ref      = ref 0  (* Сколько еще элементов прочитать осталось в FreeServerReadMode *)


    (* Пробегаем по всем uniq_s_addrs и, если нет base_reply, парсим buf и результат кладем в base_reply.
    Если для всех uniq_s_addrs есть уже base_reply, то идем дальше *)
    fun parse_bufs uniq_s_addrs =
      let
        fun parse_buf { base_reply = ref (SOME _), ... } = 1
          | parse_buf (data as { base_reply = base_reply as ref NONE, buf = buf, ... }) =
            if Substring.isEmpty (!buf) then 0 else (
              case server_parser (!buf) of
                  Done (r, t) => (base_reply := SOME r; buf := t; 1)
                | Partial => (s_enable_force data ; 0)
                | Fail    => (s_proto_error (); 0)
             )
      in
        foldl (fn (s_addr, cnt) => let val data = Vector.sub (s_data, s_addr) in cnt + parse_buf data end) 0 uniq_s_addrs
      end


    and read_bulk (data as { buf = buf, ... }) =
      case server_parser (!buf) of
          Done (r, t) => ( case r of
                                RBulk r => ( buf := t; c_send (arg2stream r); true )
                              | _ => false (* Уже другой команды данные *)
                         )
        | Partial => ( s_enable_force data; false )
        | Fail    => ( s_proto_error (); false )



    and do_base_read (RCmd (cmd, s_addrs), uniq_s_addrs) =
      let
        val is_unsupported_command = case cmdType cmd of
             NONE => (c_send_and_flush ["-ERR unsupported command '" ^ cmd ^ "'\r\n"]; clean_current_cmd (); true)
           | _    => false

        fun base_reply s_addr = valOf ( ! (#base_reply (Vector.sub (s_data, s_addr))))

        fun base_reply_and_delete s_addr =
          let
            val base_reply = (#base_reply (Vector.sub (s_data, s_addr)))
            val r = valOf ( ! base_reply )
          in
            base_reply := NONE;
            r
          end

        fun first_base_reply uniq_s_addrs = base_reply (case uniq_s_addrs of [] => 1 | _ => List.hd uniq_s_addrs)

        fun do_RMultiSize fmrs =
          if List.length uniq_s_addrs = 1 andalso fmrs = ~1
          then (c_send (arg2stream NONE); clean_current_cmd (); to_current_cmd () )
          else
            let
              fun get_size read_mode =
                let
                  fun size_from_base_reply { base_reply = base_reply as ref (SOME (RMultiSize size)), do_item_cnt = do_item_cnt, ... } =
                      (
                          ( if read_mode = FreeServerReadMode then do_item_cnt := size else () );
                          base_reply := NONE;
                          size
                      )
                    | size_from_base_reply _ = 0
                in
                  foldl ( fn (s_addr, size) => size + size_from_base_reply (Vector.sub (s_data, s_addr)) ) 0 uniq_s_addrs
                end

              val rm = case valOf(cmdType cmd) of
                            CmdToAll => FreeServerReadMode
                          | CmdToOne => OneServerReadMode
                          | _        => MultiServerReadMode


              val size = get_size rm

            in
              c_send (size2stream size);
              if size < 1
              then (
                clean_current_cmd ();
                to_current_cmd ()
              )
              else (
                read_mode := rm;
                case rm of
                    FreeServerReadMode => (
                      free_size_todo := size;
                      do_free_read_all ()
                    )
                  | OneServerReadMode => (
                      one_read := (size, (List.hd s_addrs));
                      do_one_read_all ()
                    )
                  | _ => (
                      multi_read_s_addrs:= s_addrs;
                      do_multi_read_all ()
                    )
              )
            end

        fun do_RInline fr =
          let
            val rs = List.map (fn s_addr => (case (base_reply_and_delete s_addr) of RInline r => r | _ => "-ERR RInline") ) uniq_s_addrs
          in
            if List.all (fn r => r = fr) rs
            then c_send_and_flush [fr, "\r\n"] (* Ответы идентичны. *)
            else c_send_and_flush ["-ERR nodes return different results\r\n"] (* Ответы отличаются. *)
          end

        fun do_RInt fr = (* Числовой ответ складываем. *)
          let
            val sum = List.foldl (fn (s_addr, sum) => (case (base_reply_and_delete s_addr) of RInt r => (r + sum) | _ => 0) ) 0 uniq_s_addrs
          in
            c_send_and_flush [":", (Int.toString sum), "\r\n"]
          end

       fun do_RBulk fmr = (* Кажется все эти команды должны быть с одного сервера. *)
         if valOf(cmdType cmd) <> CmdToOne then printLog ("bulk cmd " ^ cmd ^ " with not CmdToOne type") else
         if (List.length s_addrs) <> 1 then printLog ("logic error for" ^ cmd) else
         (
         base_reply_and_delete (List.hd s_addrs);
         c_send_and_flush (arg2stream fmr)
         )

      in
        (* print (cmd ^ " cmd from\n"); *) (* ToDo *)
        if is_unsupported_command then () else
        if parse_bufs uniq_s_addrs <> List.length uniq_s_addrs then () else
        (
          (
          case first_base_reply uniq_s_addrs of
               RInt _          => ( do_RInt ();    clean_current_cmd (); to_current_cmd () )
             | RInline fr      => ( do_RInline fr; clean_current_cmd (); to_current_cmd () )
             | RBulk fmr       => ( do_RBulk fmr;  clean_current_cmd (); to_current_cmd () )
             | RMultiSize fmrs => ( do_RMultiSize fmrs )
          )
        )
      end


    (* s_enable_all - пробегает по всем серверам, выбирает только не активные, и для тех, у кого в буфере нет данных *)
    and s_enable_all () =
      let
        fun s_enable data = if !(#actived data) then () else if not (Substring.isEmpty (!(#buf data))) then () else
          ( evModify ev [evAdd ((#fd data), evRead, (s_cb data))] ; (#actived data) := true )
      in
        vector_for s_enable s_data
      end

    (* Активировать чтение, даже если в буфере есть данные *)
    and s_enable_force data = if !(#actived data) then () else
      ( evModify ev [evAdd ((#fd data), evRead, (s_cb data))] ; (#actived data) := true )

    and s_disable data = if ( !(#actived data) andalso not (Substring.isEmpty (!(#buf data))) ) then
      ( evModify ev [evDelete ((#fd data), evRead)] ; (#actived data) := false ) else ()

    and s_disable_all () = vector_for s_disable s_data

    and s_cb data (fd, _) =
      let
        val s = recvString (#sock data)
      in
        if s = ""
        then clean () (* s вернул пустую строку - закрылся *)
        else (
            (* (#buf data) := Substring.full ( Substring.string (!(#buf data)) ^ s ); *)
            (#buf data) := Substring.full ( String.^ ( Substring.string (!(#buf data)),  s ) );
            (case !read_mode of
                  BaseServerReadMode  => to_current_cmd ()
                | MultiServerReadMode => do_multi_read_all ()
                | OneServerReadMode   => do_one_read_all ()
                | FreeServerReadMode  => do_free_read_all ()
            );
            (* ToDo а вот тут, если s_disable data, то s_enable_all потом не включит,
            так как в буфере есль кусок данных
            redis-benchmark -p 8080 -n 10 -c 1 -q -t get -P 10
            Когда send_size и recv_size = 1 * 10
            s_disable data;
            *)
            s_enable_all ()
            )
      end

    and c_enable  () = if !c_actived = false then ( evModify ev [evAdd (c_fd, evWrite, c_cb)] ;  c_actived := true   ) else ()
    and c_disable () = if !c_actived = true  then ( evModify ev [evDelete (c_fd, evWrite)] ;     c_actived := false  ) else ()

    and c_cb (fd, _) =
      let
        val buf = !c_buf
        val strToSlc = Word8VectorSlice.full o Byte.stringToBytes
        val cnt = Socket.sendVec (c_sock, (strToSlc buf)) handle (OS.SysErr _) => 0
        val tail = String.extract (buf, cnt, NONE)
      in
        c_buf := tail;
        if tail = ""
        then (c_disable (); s_enable_all () )
        else ()
      end

    and to_current_cmd () = case !current_cmd of
        SOME (cmd, uniq_s_addrs) => do_base_read (cmd, uniq_s_addrs)
      | NONE     => (
         case get_cmd () of
             NONE => c_flush ()
           | SOME (cmd as (RCmd (c, s_addrs))) => (
               let
                 val uniq_s_addrs = isolate s_addrs
               in
                 current_cmd := SOME (cmd, uniq_s_addrs);
                 do_base_read (cmd, uniq_s_addrs)
               end
             )
         )

    and do_multi_read_all () =
      let
        fun loop [] = (multi_read_s_addrs := []; clean_current_cmd (); to_current_cmd ())
          | loop (l as i::t) =
            let
              val data = Vector.sub (s_data, i)
            in
              if Substring.isEmpty (! (#buf data)) then multi_read_s_addrs := l else
              if read_bulk data then loop t else multi_read_s_addrs := l
            end
      in
        loop (!multi_read_s_addrs)
      end

    and do_one_read_all () =
      let
        val (size, addr) = (!one_read)
        val data = Vector.sub (s_data, addr)
        fun loop 0 = (one_read := (0, 0); clean_current_cmd (); to_current_cmd ())
          | loop n =
            if Substring.isEmpty (! (#buf data)) then one_read := (n, addr) else
            if read_bulk data then loop (n - 1) else one_read := (n, addr)
      in
        loop size
      end

    and do_free_read_all () =
      let
        fun read_and_send (data as { buf = ref buf, do_item_cnt = ref do_item_cnt, ... } ) =
          if Substring.isEmpty buf then 0 else
          if do_item_cnt = 0 then 0 else
          let
            fun do_while_true f a = let fun doit n = if f a then doit (n + 1) else n in doit 0 end
            val cnt = do_while_true read_bulk data
          in
            (#do_item_cnt data) := do_item_cnt - cnt;
            cnt
          end

        val size = Vector.foldl (fn (data, size) => size + read_and_send data ) 0 s_data
      in
        free_size_todo := (!free_size_todo) - size;
        if (!free_size_todo) = 0 then ( clean_current_cmd (); to_current_cmd () ) else ()
      end


    and c_flush () =
      let
        val s = ListBuf.toString (!c_channel)
        val _ = c_channel := ListBuf.empty
      in
        c_buf := String.^((!c_buf), s);
        c_enable ();
        if String.size (!c_buf) > recv_size * 10 then s_disable_all () else ()
      end

    and c_send ss =
      let
        val size = foldl (fn(s, size) => size + String.size s) 0 ss
      in
        c_channel := ListBuf.add(!c_channel) ss;
        if ListBuf.size(!c_channel) >= send_size
        then c_flush ()
        else ()
      end

   and c_send_and_flush s = (c_send s; c_flush (); set_time_last_activity ())

   and s_proto_error () =
     let val msg = "-ERR unified protocol error from server" in
     printLog msg; c_send_and_flush [msg, "\r\n"]; clean () end

  in
    (s_enable_all, c_send_and_flush )
  end


fun main_handle' (listen_sock, servers, client_timeout, N) =
  let

    (* ToDo *)
    val ev_timeout = Time.fromSeconds 3
    val timeout_hash_size = 100
    val timeout_clean_interval = Time.fromSeconds 10

    (* val stop = ref 0 *)

    val now = ref (Time.now ())

    val ev = evInit ()


    fun server_connect_info (host, port) =
      let
        val h = valOf(NetHostDB.fromString host)
        val addr = INetSock.toAddr(h, port)
        val sock = INetSock.TCP.socket()
      in
        Socket.Ctl.setKEEPALIVE (sock, true);
        (sock, addr)
      end


    val timeout_hash : ((Time.time ref) * (unit -> unit)) H.hash = H.hash timeout_hash_size

    fun welcome c_sock =
    let
      val c_fd = sockToEvFD c_sock
      val c_info = (c_sock,c_fd)

      val s_count = Vector.length servers
      val s_socks_info = Vector.map server_connect_info servers (* (sock, addr) *)
      val s_socks = Vector.map (fn (sock, addr) => sock) s_socks_info
      val s_info  = Vector.map (fn (sock, addr) => (sock, sockToEvFD sock)) s_socks_info (* (sock, fd) *)

      fun connect () = vector_for (fn (sock, addr) => (Socket.connectNB(sock, addr); ())) s_socks_info

      val time_last_activity = ref (!now)
      val set_time_last_activity = fn () => time_last_activity := !now

      val cleaned = ref false
      fun clean () =
        if (!cleaned) then () else
        (* Предотвращаем повторный вызов кода в clean, а повторный вызов возможен,
           когда в одном буфере, есть несколько ошибочных данных.
           Хотя, наверное, будет правильным предотвратить это там, где обрабатывается буфер:
           сделать выход из fold по исключению и ловить снаружи.
         *)
        let
          val clean_s = Vector.foldr (fn ((_,fd),r) => evDelete (fd, evRead) :: evDelete (fd, evWrite) :: r) [] s_info
        in
          evModify ev (evDelete (c_fd, evRead) :: evDelete (c_fd, evWrite) :: clean_s);
          Vector.app Socket.close s_socks;
          Socket.close (c_sock);
          H.delete(timeout_hash, c_fd);
          cleaned := true
          (* ; stop := (!stop) + 1 *)
        end

      val _ = H.update(timeout_hash, c_fd, (time_last_activity, clean))

      local
        val cmds_set : RCmd list ref = ref []
        val cmds_get : RCmd list ref = ref []
      in
        fun set_cmd c = ( cmds_set := c::(!cmds_set) ; time_last_activity := Time.zeroTime )

        fun get_cmd () =
         case (!cmds_get) of
              (x::xs) => ( cmds_get := xs; SOME x)
            | nil     => case (!cmds_set) of nil => NONE | _ => (cmds_get := List.rev (!cmds_set) ; cmds_set := [] ; get_cmd () )
      end
      (*
      Если неизвестная команда, то все равно set_cmd.
      А когда get_cmd в servers_stream, кто в client посылается ответ,
      что неизвестная команда.
      *)

      val connected = ( connect (); true ) handle exc => ( clean () ; printLog ("function connect raised an exception: " ^ exnMessage exc); false )

      val (servers_stream_up, c_send_and_flush) = servers_stream ev c_info s_info get_cmd clean set_time_last_activity
      (* servers_stream_up вызывается из client_stream после set_cmd. servers_stream_up активирует чтение с серверов. *)

    in
      if connected = true
      then client_stream ev c_info s_info set_cmd clean servers_stream_up c_send_and_flush
      else ()
    end

    fun accepter _ =
      case (Socket.acceptNB (listen_sock)) of
           NONE => () (* Это случается, когда несколько детей слушают одновременно *)
         | SOME (c_sock, _) => (
              Socket.Ctl.setKEEPALIVE (c_sock, true);
              (welcome c_sock) handle exc => printLog ("function welcome raised an exception: " ^ exnMessage exc)
            )

    val fd = sockToEvFD listen_sock
    val _ = evModify ev [evAdd (fd, evRead, accepter)]


    val last_timeout_clean = ref (!now)
    fun timeout_clean () =
      let
        val now_time = (!now)

        fun is_timeout(now, last, timeout) = Time.< ((Time.- (now, last)), timeout)

        fun is_activity time_last_activity =
          if time_last_activity = Time.zeroTime then true else
          if is_timeout(now_time, time_last_activity, client_timeout) then true else false
      in
        if client_timeout = Time.zeroTime then () else
        if is_timeout(now_time, (!last_timeout_clean), timeout_clean_interval) then () else
          H.fold (fn (c_fd, (time_last_activity, clean), _) => (
              if is_activity (!time_last_activity) then () else clean ()
            ) ) () timeout_hash
       end


    fun loop () =
      let
        val wait_cnt = evWait ev (SOME ev_timeout)
      in
        if wait_cnt = ~1 then loop () else
        now := Time.now ();
        timeout_clean ();
        (* if N <=1 andalso (!stop) >= 30 then print "STOP\n" else *)
        loop ()
      end

  in
    loop () handle exc => printLog ("function loop raised an exception: " ^ exnMessage exc)
  end


fun main_handle () =
  case getParam (CommandLine.arguments ()) of NONE => () | SOME (listen_host, listen_port, nodes, timeout, N, SO_REUSEPORT) =>
  let
    val servers = Vector.fromList nodes
    val client_timeout = Time.fromSeconds timeout

    (* ToDo *)
    val listen_cnt = 10

    fun listen (host, port) =
      let
        val h = valOf(NetHostDB.fromString host)
        val addr = INetSock.toAddr(h, port) (* val addr = INetSock.any port *)
        val sock = INetSock.TCP.socket ()
        val fd = sockToEvFD sock
      in
        printLog ("Listening on " ^ host ^ ":" ^ (Int.toString port) ^ ".");
        Socket.Ctl.setREUSEADDR (sock, true);
        if SO_REUSEPORT = 0 then () else setsockopt_REUSEPORT fd; (* for Linux 3.9 or DragonFlyBSD *)
        Socket.bind (sock, addr);
        Socket.listen (sock, listen_cnt);
        sock
      end

    val listen_sock = listen (listen_host, listen_port )
  in
    runWithN N main_handle' (listen_sock, servers, client_timeout, N);
    Socket.close (listen_sock);
    printLog "The End"
  end


val version = "3.1"

fun main' () = (
  printLog ("Start RedisSharding SML, (version - " ^ version ^ ").");
  main_handle () handle exc => printLog ("function main raised an exception: " ^ exnMessage exc)
  )

fun main () = main_wrapper main' ()
