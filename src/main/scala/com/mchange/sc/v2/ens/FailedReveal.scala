package com.mchange.sc.v2.ens

import scala.collection._

import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash}


case class FailedReveal( failure : Throwable, bid : Bid )
