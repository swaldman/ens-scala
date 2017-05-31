package com.mchange.sc.v2.ens

import scala.collection._

import com.mchange.sc.v1.consuela.ethereum.EthHash

object BidStore {
  final object State {
    final case object Created   extends State
    final case object Accepted  extends State
    final case object Revealed  extends State

    private val upperStringToState = Map[String,State]( "CREATED" -> Created, "ACCEPTED" -> Accepted, "REVEALED" -> Revealed )

    def fromString( str : String ) : Option[State] = upperStringToState.get( str.toUpperCase )
  }
  sealed trait State
}

/**
  *  All methods can throw Exceptions!
  */ 
trait BidStore {
  def store( bid : Bid ) : Unit
  def remove( bid : Bid ) : Unit
  def markAccepted( bidHash : EthHash ) : Unit
  def markRevealed( bidHash : EthHash ) : Unit
  def findByHash( bidHash : EthHash ) : ( Bid, BidStore.State )
  def findByName( simpleName : String ) : immutable.Seq[( Bid, BidStore.State )]
}
