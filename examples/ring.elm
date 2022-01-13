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

type Cmp
  = LT ()
  | EQ ()
  | GT ()

compare_nat : Nat -> Nat -> Cmp
compare_nat n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> EQ ()
        S _ -> LT ()
    S m1 ->
      case n2 of
        Z _  -> GT ()
        S m2 -> compare_nat m1 m2


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
        Send q -> and (check (#4.4 p)) ??

specifyFunction check
  [ (Empty (), True ())
  , (Send(Client(), Server (), Request(1), Empty ()), True ())
  , (Send(Server (), Client(), Response(1, False ()), Send(Client(), Server (), Request(1), Empty ())), True ())
  ]
