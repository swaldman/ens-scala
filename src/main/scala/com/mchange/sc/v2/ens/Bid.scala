package com.mchange.sc.v2.ens

import scala.collection._

import com.mchange.sc.v1.consuela.ethereum.EthHash


case class Bid( bidHash : EthHash, simpleName : String, nameHash : EthHash, valueInWei : BigInt, salt : immutable.Seq[Byte], timestamp : Long )
