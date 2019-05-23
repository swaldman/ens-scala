package com.mchange.sc.v2.ens

import com.mchange.sc.v1.consuela.ethereum.EthAddress

// XXX: Not very DRY about "reverse" :: "addr" <=> "addr.reverse"
final object ParsedPath {
  case class Subnode private [ParsedPath] ( componentsReversed : List[String] ) extends HasBaseName {
    assert( componentsReversed.length >= 3, s"Subnodes should have three or more components, but found ${componentsReversed}." )

    lazy val subnode : String = componentsReversed.drop(2).reverse.mkString(".")
  }
  case class BaseNameTld private [ParsedPath] ( componentsReversed : List[String] ) extends HasBaseName {
    assert( componentsReversed.length == 2, s"BaseNameTlds should have two components, but found ${componentsReversed}." )
  }
  case class Tld private [ParsedPath] ( componentsReversed : List[String] ) extends Forward {
    assert( componentsReversed.length == 1, s"Forward Tlds should have just one component, but found ${componentsReversed}." )
  }
  case class ReversePath private [ParsedPath] ( componentsReversed : List[String] ) extends Reverse {
    assert( componentsReversed.length == 3 && componentsReversed.take(2) == "reverse" :: "addr" :: Nil, s"Unexpected reverse namespace! [componentsReversed: ${componentsReversed}]" )

    lazy val address = EthAddress( componentsReversed.last )
  }
  case object ReverseTld extends Reverse {
    val componentsReversed = "reverse" :: "addr" :: Nil
  }
  sealed trait HasBaseName extends Forward {
    lazy val baseName : String = componentsReversed.tail.head
    lazy val parent : ParsedPath.Forward = ParsedPath.Forward( components.tail.reverse )
    lazy val label : String = components.head

    def baseNameTld : Tuple2[String,String] = ( baseName, tld )
  }
  final object Forward {
    def apply( path : String ) : ParsedPath.Forward = {
      val componentsReversed = tokenizeReverse( path )
      this.apply( componentsReversed )
    }
    def apply( componentsReversed : List[String] ) : ParsedPath.Forward = {
      componentsReversed.length match {
        case 0 => throw new BadEnsPathException( s"Empty path is not a valid forward ENS name." )
        case 1 => Tld( componentsReversed )
        case 2 => BaseNameTld( componentsReversed )
        case _ => Subnode( componentsReversed )
      }
    }
  }
  sealed trait Reverse extends ParsedPath {
    lazy val reverseTld = componentsReversed.take(2).reverse.mkString(".")
  }
  sealed trait Forward extends ParsedPath {
    lazy val topLevelDomain : String = componentsReversed.head
    def tld : String = this.topLevelDomain
  }
  def apply( path : String ) : ParsedPath = {
    val componentsReversed = tokenizeReverse( path )
    this.apply( componentsReversed )
  }
  def apply( componentsReversed : List[String] ) : ParsedPath = {
    componentsReversed match {
      case Nil                             => throw new BadEnsPathException( s"Empty path is not a valid ENS name." )
      case "reverse" :: "addr" :: _ :: Nil => ReversePath( componentsReversed )
      case "reverse" :: "addr" :: Nil      => ReverseTld
      case "reverse" :: "addr" :: _        => throw new BadEnsPathException( s"""Unsupported reverse path (too many elements): ${componentsReversed.reverse.mkString(".")}""" )
      case _                               => Forward( componentsReversed )
    }
  }
}
sealed trait ParsedPath {
  def componentsReversed : List[String]

  lazy val components = componentsReversed.reverse

  lazy val fullPath = components.mkString(".")

  def fullName = fullPath

  lazy val namehash = namehashReverseComponents( componentsReversed )

  override def toString = super.toString() + s"[fullPath=${fullPath}]"
}
