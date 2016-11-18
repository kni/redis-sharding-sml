open Sparcl

datatype CmdType = 
   CmdToAll         (* На все сервера *)
 | CmdToOne         (* На конкретные сервер *)
 | CmdToMany        (* На множество серверов. CMD key1 ... keyN *)
 | CmdToManyValues  (* На множество серверов. CMD key1 value1 ... keyN valueN *)
 | CmdToManyTimeout (* На множество серверов. CMD key1 ... keyN timeout (блокирующие команды)*)

fun cmdType "PING"             = SOME CmdToAll
  | cmdType "AUTH"             = SOME CmdToAll
  | cmdType "SELECT"           = SOME CmdToAll
  | cmdType "FLUSHDB"          = SOME CmdToAll
  | cmdType "FLUSHALL"         = SOME CmdToAll
  | cmdType "DBSIZE"           = SOME CmdToAll
  | cmdType "KEYS"             = SOME CmdToAll

  | cmdType "EXISTS"           = SOME CmdToOne
  | cmdType "TYPE"             = SOME CmdToOne
  | cmdType "EXPIRE"           = SOME CmdToOne
  | cmdType "PERSIST"          = SOME CmdToOne
  | cmdType "TTL"              = SOME CmdToOne
  | cmdType "MOVE"             = SOME CmdToOne
  | cmdType "SET"              = SOME CmdToOne
  | cmdType "GET"              = SOME CmdToOne
  | cmdType "GETSET"           = SOME CmdToOne
  | cmdType "SETNX"            = SOME CmdToOne
  | cmdType "SETEX"            = SOME CmdToOne
  | cmdType "INCR"             = SOME CmdToOne
  | cmdType "INCRBY"           = SOME CmdToOne
  | cmdType "INCRBYFLOAT"      = SOME CmdToOne
  | cmdType "DECR"             = SOME CmdToOne
  | cmdType "DECRBY"           = SOME CmdToOne
  | cmdType "APPEND"           = SOME CmdToOne
  | cmdType "SUBSTR"           = SOME CmdToOne
  | cmdType "RPUSH"            = SOME CmdToOne
  | cmdType "LPUSH"            = SOME CmdToOne
  | cmdType "LLEN"             = SOME CmdToOne
  | cmdType "LRANGE"           = SOME CmdToOne
  | cmdType "LTRIM"            = SOME CmdToOne
  | cmdType "LINDEX"           = SOME CmdToOne
  | cmdType "LSET"             = SOME CmdToOne
  | cmdType "LREM"             = SOME CmdToOne
  | cmdType "LPOP"             = SOME CmdToOne
  | cmdType "RPOP"             = SOME CmdToOne
  | cmdType "SADD"             = SOME CmdToOne
  | cmdType "SREM"             = SOME CmdToOne
  | cmdType "SPOP"             = SOME CmdToOne
  | cmdType "SCARD"            = SOME CmdToOne
  | cmdType "SISMEMBER"        = SOME CmdToOne
  | cmdType "SMEMBERS"         = SOME CmdToOne
  | cmdType "SRANDMEMBER"      = SOME CmdToOne
  | cmdType "ZADD"             = SOME CmdToOne
  | cmdType "ZREM"             = SOME CmdToOne
  | cmdType "ZINCRBY"          = SOME CmdToOne
  | cmdType "ZRANK"            = SOME CmdToOne
  | cmdType "ZREVRANK"         = SOME CmdToOne
  | cmdType "ZRANGE"           = SOME CmdToOne
  | cmdType "ZREVRANGE"        = SOME CmdToOne
  | cmdType "ZRANGEBYSCORE"    = SOME CmdToOne
  | cmdType "ZCOUNT"           = SOME CmdToOne
  | cmdType "ZCARD"            = SOME CmdToOne
  | cmdType "ZSCORE"           = SOME CmdToOne
  | cmdType "ZREMRANGEBYRANK"  = SOME CmdToOne
  | cmdType "ZREMRANGEBYSCORE" = SOME CmdToOne
  | cmdType "HSET"             = SOME CmdToOne
  | cmdType "HGET"             = SOME CmdToOne
  | cmdType "HMGET"            = SOME CmdToOne
  | cmdType "HMSET"            = SOME CmdToOne
  | cmdType "HINCRBY"          = SOME CmdToOne
  | cmdType "HEXISTS"          = SOME CmdToOne
  | cmdType "HDEL"             = SOME CmdToOne
  | cmdType "HLEN"             = SOME CmdToOne
  | cmdType "HKEYS"            = SOME CmdToOne
  | cmdType "HVALS"            = SOME CmdToOne
  | cmdType "HGETALL"          = SOME CmdToOne
  | cmdType "PUBLISH"          = SOME CmdToOne

  | cmdType "DEL"              = SOME CmdToMany
  | cmdType "MGET"             = SOME CmdToMany
  | cmdType "SUBSCRIBE"        = SOME CmdToMany
  | cmdType "UNSUBSCRIBE"      = SOME CmdToMany

  | cmdType "MSET"             = SOME CmdToManyValues
  | cmdType "MSETNX"           = SOME CmdToManyValues

  | cmdType "BLPOP"            = SOME CmdToManyTimeout
  | cmdType "BRPOP"            = SOME CmdToManyTimeout

  | cmdType _                  = NONE


val _ = fn (cmd:string) => (cmdType cmd):(CmdType option)

(* ToDo и для Haskell варианта сделать cmdType *)





val endOfLine = takeStr "\r\n"


fun get_bulk_size c = takeStr c *> takeInt <* endOfLine


fun get_bulk_value ~1 = pure NONE
  | get_bulk_value n  = SOME <$> takeN n <* endOfLine


val get_bulk_arg = get_bulk_size "$" >>= get_bulk_value


val multi_bulk_parser : string option list option Parser = 
  let
    fun get_args args ~1 = pure NONE
      | get_args args 0  = pure (SOME (List.rev args))
      | get_args args n  = get_bulk_arg >>= (fn a => get_args (a::args) (n - 1))
  in
    get_bulk_size "*" >>= get_args []
  end



datatype Reply = RInt of int | RInline of string | RBulk of string option | RMultiSize of int

fun showReply (RInt i)         = ("RInt " ^ Int.toString i)
  | showReply (RInline s)      = ("RInline " ^ s)
  | showReply (RBulk NONE)     = "RBulk NONE"
  | showReply (RBulk (SOME s)) = ("RBulk (SOME " ^ s ^ ")")
  | showReply (RMultiSize i)   = ("RMultiSize " ^ Int.toString i)

val server_parser : Reply Parser =
  let
    val line            = RInline <$> (choice [takeStr "+", takeStr "-"] >>= (fn h => takeBefore "\r\n" >>= (fn t => endOfLine *> pure (h ^ t))))
    val integer         = RInt       <$> (takeStr ":" *> takeInt <* endOfLine)
    val bulk            = RBulk      <$> get_bulk_arg
    val multi_bulk_size = RMultiSize <$> get_bulk_size "*"
  in
    choice [line, integer, bulk, multi_bulk_size]
  end


val server_parser_bulk: Reply Parser = RBulk <$> get_bulk_arg



fun size2stream size = ["*", (Int.toString size), "\r\n"]

(* Преобразование аргумента в строку, поток байтов, соответствующий протоколу redis. *)
fun arg2stream NONE     = ["$-1\r\n"]
  | arg2stream (SOME s) = ["$", (Int.toString (String.size s)), "\r\n", s, "\r\n"]


(* Преобразование команды (список аргументов) в строку, поток байтов, соответствующий протоколу redis. *)
fun cmd2stream [] = ["*0\r\n"]
  | cmd2stream l  =
    let
      val h = ["*", (Int.toString (List.length l)), "\r\n"]
      val t = map arg2stream l
    in
       h @ (List.concat t)
    end
