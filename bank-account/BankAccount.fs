//             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                     Version 2, December 2004
//
//  Copyright (C) 2016 Eric Bailey <eric@ericb.me>
//
//  Everyone is permitted to copy and distribute verbatim or modified
//  copies of this license document, and changing it is allowed as long
//  as the name is changed.
//
//             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//   0. You just DO WHAT THE FUCK YOU WANT TO.
//

module BankAccount

type BankAccount =
  | Open of float
  | Closed

let mkBankAccount() = Closed

let openAccount = function
  | Open balance -> Open balance
  | Closed       -> Open 0.0

let getBalance = function
  | Open balance -> Some balance
  | Closed       -> None

let updateBalance (amount: float) = function
  | Open balance -> Open (balance + amount)
  | Closed       -> Closed

let closeAccount _ = Closed

(*
// http://fssnip.net/19
let ct   a _   = a
let flip f x y = f y x

type BankAccount = float option

let mkBankAccount() : BankAccount = None

let openAccount : BankAccount -> BankAccount = Some << Option.fold (flip ct) 0.0

let getBalance : BankAccount -> BankAccount = id

let updateBalance : float -> BankAccount -> BankAccount = Option.map << (+)

let closeAccount : BankAccount -> BankAccount = ct None
*)
