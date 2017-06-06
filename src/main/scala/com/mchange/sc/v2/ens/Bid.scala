package com.mchange.sc.v2.ens

import scala.collection._

import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash}


case class Bid( bidHash : EthHash, simpleName : String, bidderAddress : EthAddress, valueInWei : BigInt, salt : immutable.Seq[Byte] )
