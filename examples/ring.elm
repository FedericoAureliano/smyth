type Bool
  = False ()
  | True ()

type Nat
  = Z ()
  | S Nat

type Event
  = Request Nat
  | Response (Nat, Bool)

type MRef
  = Client ()
  | Server ()

type EventList
  = Empty ()
  | Send (MRef, MRef, Event, EventList)

and : Bool -> Bool -> Bool
and a b = 
  case a of
    False _ -> False
    True _ ->
      case b of
        False _ -> False
        True _ -> True

eq_nat : Nat -> Nat -> Bool
eq_nat n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> True ()
        S _ -> False ()
    S a ->
      case n2 of
        S b -> eq_nat a b 
        Z _ -> False ()

eq_mref : MRef -> MRef -> Bool
eq_mref n1 n2 =
  case n1 of
    Client _ ->
      case n2 of
        Client _ -> True ()
        Server _ -> False ()
    Server _ ->
      case n2 of
        Server _ -> True ()
        Client _ -> False ()

check : EventList -> Bool
check e =
  case e of
    Empty _ -> True
    Send p -> 
      case (#4.4 p) of
        Empty _ -> True
        Send q -> 
          let
            a : Nat
            a = 
            case (#4.3 p) of 
              Request i -> i
              Response ij -> (#2.1 ij)
          in
          let
            b : Nat
            b = 
            case (#4.3 q) of 
              Request i -> i
              Response ij -> (#2.1 ij)
          in
          and (check (#4.4 p)) ??

specifyFunction check
  [ (Empty (), True ())
  , (Send(Client (), Server (), Request(1), Empty ()), True ())
  , (Send(Server (), Client (), Response(1, False ()), Send(Client (), Server (), Request(1), Empty ())), True ())
  , (Send(Server (), Client (), Response(1, False ()), Send(Client (), Client (), Request(0), Empty ())), False ())
  ]
